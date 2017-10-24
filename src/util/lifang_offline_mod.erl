-module(lifang_offline_mod).
-include("include/ejabberd.hrl").
-include("include/jlib.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include("include/logger.hrl").

-export([make_offline/3,get_offline_msg/2]).

%% 离线消息对象
%% -record(offline_msg, {us, timestamp, expire, from, to, packet}).
-define(NORMAL_EXPIRE,60*60*24*7).
-define(LITTLE_SECRETARY_EXPIRE,60*60*24*2).


%% =================================================
%% 获取不到msgid对应的消息时，
%% 会选择在 Sorted Set 中删除该条key
%% =================================================

get_offline_msg(#jid{user=User,server=Domain,resource=Resource},Offline_msgid)->
  KEY = <<User/binary, "@", Domain/binary, "/offline_msg">>,
  try  
      %% 如果是小秘书帐户，需要过滤hset中的key，如果有则不需要发送
      SEND = case aa_hookhandler:is_little_secretary(User) of
        true ->
          HK = <<User/binary, "@", Domain/binary, "/", Resource/binary>>,
          case HK = <<User/binary, "@", Domain/binary, "/", Resource/binary>> of 
            {ok,undefined} ->
              true;
            _ ->
              false
          end;
        false ->
          true
      end,
      ?INFO_MSG("need send ~p~n",[{SEND,Offline_msgid}]),
      case SEND of 
        true ->      
            case aa_hookhandler:ecache_cmd( ["GET",Offline_msgid] ) of 
                {ok,Obj} when erlang:is_binary(Obj) ->
                   {From,To,Packet} =  erlang:binary_to_term(Obj),
                   {xmlel,<<"message">>,Attr,Body} = Packet,
                   {From,To,{xmlel,<<"message">>,[{<<"isoff">>,<<"1">>}|Attr],Body}};
                _R->
                  ?DEBUG("miss msg :~p ",[_R]),
                  CMD = ["ZREM",KEY,Offline_msgid],
                  aa_hookhandler:ecache_cmd(CMD),
                  skip
               end;
        _->
          skip
      end
  catch
    E:R->
      ?ERROR_MSG("get offline msg~p",[{E,R,erlang:get_stacktrace()}])
  end.


%% =================================================
%% 单聊离线打包
%% =================================================

make_offline(normalchat,{NormalChatoffline,From,To,Packet},{Idlist,MsgId})->
  ?DEBUG("make offline .. normalchat ~n\t NormalChatoffline = ~p",[NormalChatoffline]),
  case lists:keyfind(From, 1, NormalChatoffline) of
    false->
      {xmlel,<<"message">>,Attr,_} = Packet,
      MsgTime = fxml:get_tag_attr_s(<<"msgTime">>, Packet),
      OLdid = fxml:get_tag_attr_s(<<"id">>, Packet),
      {M,S,SS} = now(),
      NewID = erlang:integer_to_binary(M*1000000000000+S*1000000+SS),
      Attrs0 = lists:keydelete(<<"msgTime">>, 1, Attr),
      Attrs1 = lists:keydelete(<<"id">>, 1, Attrs0),
      Attrs2 = [{<<"id">>, NewID} | Attrs1],
      [JSON] = aa_log:get_text_message_from_packet(Packet),  
      StructJson = jiffy:decode(erlang:list_to_binary(JSON)),
      % -------------------------------------------------
      % 为压缩包中每条信息取出并删除json中的username/userid/image等字段
      % 为压缩包中每条信息添加msgid id msgtime字段
      % -------------------------------------------------
      Userid = ej:get({<<"userid">>},StructJson),
      Username = ej:get({<<"username">>},StructJson),
      Userimage = ej:get({<<"userimage">>},StructJson),
      Usergender = ej:get({<<"usergender">>},StructJson),

      StructJson2 = ej:delete({<<"userid">>},StructJson),
      StructJson3 = ej:delete({<<"username">>},StructJson2),
      StructJson4 = ej:delete({<<"userimage">>},StructJson3),
      StructJson5 = ej:delete({<<"usergender">>},StructJson4),

      StructJson6 = ej:set({<<"msgid">>},StructJson5,OLdid),
      StructJson7 = ej:set({<<"id">>},StructJson6,OLdid),
      StructJson8 = ej:set({<<"msgTime">>},StructJson7,MsgTime),
      % -------------------------------------------------
      % 为压缩包添加同意的userid、username等
      % 为压缩包json中添加offlinelist字段
      % -------------------------------------------------

      J1 = set_offline_Json_field({[]},"userid",Userid),
      J2 = set_offline_Json_field(J1,"username",Username),
      J3 = set_offline_Json_field(J2,"userimage",Userimage),
      J4 = set_offline_Json_field(J3,"usergender",Usergender),
      J5 = ej:set({<<"type">>},J4, <<"10000">>),
      J6 = ej:set({<<"offlinelist">>},J5, [StructJson8]),

      Jb = jiffy:encode(J6),
      NewPacket = {xmlel,<<"message">>,Attrs2,[{xmlel,<<"body">>,[],[{xmlcdata,Jb}]}]},
      OfflineZipKey =  <<NewID/binary, "/zip_offline_msg_body">>,
      {[{From,To,NewPacket}|NormalChatoffline],[{OfflineZipKey,[MsgId]}|Idlist]};
    
    {_,_,Oldpacket}->
      [JSON] = aa_log:get_text_message_from_packet(Oldpacket),  
      StructJson = jiffy:decode(erlang:list_to_binary(JSON)),
      Oldofflinelist = ej:get({<<"offlinelist">>},StructJson),
      {xmlel,<<"message">>,Attr,_} = Oldpacket,
      NewID = fxml:get_tag_attr_s(<<"id">>, Oldpacket),

      MsgTime = fxml:get_tag_attr_s(<<"msgTime">>, Packet),
      OLdid = fxml:get_tag_attr_s(<<"id">>, Packet),
      [NewJSON] = aa_log:get_text_message_from_packet(Packet),  
      NewJ1 = jiffy:decode(erlang:list_to_binary(NewJSON)),

      NewJ2 = ej:delete({<<"userid">>},NewJ1),
      NewJ3 = ej:delete({<<"username">>},NewJ2),
      NewJ4 = ej:delete({<<"userimage">>},NewJ3),
      NewJ5 = ej:delete({<<"usergender">>},NewJ4),

      NewJ6 = ej:set({<<"msgid">>},NewJ5,OLdid),
      NewJ7 = ej:set({<<"id">>},NewJ6,OLdid),
      NewJ8 = ej:set({<<"msgTime">>},NewJ7,MsgTime),
      % -------------------------------------------------
      % 尾部追加新的offline json
      % -------------------------------------------------
      NewJ9 = ej:set({<<"offlinelist">>},StructJson, Oldofflinelist ++ [NewJ8]),

      Jb = jiffy:encode(NewJ9),
      NewPacket = {xmlel,<<"message">>,Attr,[{xmlel,<<"body">>,[],[{xmlcdata,Jb}]}]},
      OfflineZipKey =  <<NewID/binary, "/zip_offline_msg_body">>,
      {_,Ziplist} = lists:keyfind(OfflineZipKey, 1, Idlist),
      {lists:keyreplace(From, 1, NormalChatoffline,{From,To,NewPacket}),lists:keyreplace(OfflineZipKey, 1, Idlist, {OfflineZipKey,[MsgId|Ziplist]})}
  end;


%% =================================================
%% 讨论组离线
%% =================================================

make_offline(groupchat,{Groupchat,From,To,Packet},{Idlist,MsgId})->
  [JSON] = aa_log:get_text_message_from_packet(Packet),  
  StructJson = jiffy:decode(erlang:list_to_binary(JSON)),
  Groupid = ej:get({<<"groupid">>},StructJson),
  #jid{lserver = Domain} = From,
  Fromid = Groupid,
  FDomain  = <<"group.",Domain/binary>>,
  NewFrom = #jid{user = Fromid,server = FDomain, resource = [], luser = Fromid, lserver = FDomain, lresource = []},
  case lists:keyfind(NewFrom, 1, Groupchat) of
    false->
      {xmlel,<<"message">>,Attr,_} = Packet,
      MsgTime = fxml:get_tag_attr_s(<<"msgTime">>, Packet),
      OLdid = fxml:get_tag_attr_s(<<"id">>, Packet),
      {M,S,SS} = now(),
      NewID = erlang:integer_to_binary(M*1000000000000+S*1000000+SS),
      Attrs0 = lists:keydelete(<<"msgTime">>, 1, Attr),
      Attrs1 = lists:keydelete(<<"id">>, 1, Attrs0),
      Attrs2 = [{<<"id">>, NewID} | Attrs1],
      Groupname = ej:get({<<"groupname">>},StructJson),
      Groupmember = ej:get({<<"groupmember">>},StructJson),
      case ej:get({<<"mask">>},StructJson) of
        undefined ->
          Mask = <<"0">>;
        Mask ->
          skip
      end,

      At = ej:get({<<"at">>},StructJson),

      J1 = ej:delete({<<"groupid">>},StructJson),
      J2 = ej:delete({<<"groupname">>},J1),
      J3 = ej:delete({<<"groupmember">>},J2),
      J4 = ej:delete({<<"mask">>},J3),
      J5 = ej:delete({<<"at">>},J4),
      
      J6 = ej:set({<<"msgid">>},J5,OLdid),
      J7 = ej:set({<<"id">>},J6,OLdid),
      J8 = ej:set({<<"msgTime">>},J7,MsgTime),

      OutsideJ0 = {[]},
      %%为客户端处理at信息
      if
        At =:= undefined ->
          OutsideJat = OutsideJ0;
        true ->
          #jid{user = Toid} = To,
          case (erlang:is_binary(At) =:= false) and lists:member(Toid,At) of
            true->
              OutsideJat  = ej:set({<<"at">>},OutsideJ0, [Toid]);
            false->
              OutsideJat = OutsideJ0
          end
      end,
      OutsideJ1 = ej:set({<<"groupid">>},OutsideJat, Groupid),
      OutsideJ2 = set_offline_Json_field(OutsideJ1, "groupname", Groupname),
      OutsideJ3 = set_offline_Json_field(OutsideJ2, "groupmember", Groupmember),
      
      OutsideJ4 = ej:set({<<"type">>},OutsideJ3, <<"10000">>),
      OutsideJ5 = ej:set({<<"mask">>},OutsideJ4, Mask),
      OutsideJ6 = ej:set({<<"offlinelist">>},OutsideJ5, [J8]),
      OutsideJb = jiffy:encode(OutsideJ6),
      
      NewPacket = {xmlel,<<"message">>,Attrs2,[{xmlel,<<"body">>,[],[{xmlcdata,OutsideJb}]}]},
      OfflineZipKey =  <<NewID/binary, "/zip_offline_msg_body">>,
      {[{NewFrom,To,NewPacket}|Groupchat],[{OfflineZipKey,[MsgId]}|Idlist]};
    
    {_,_,Oldpacket}->
      [OJSON] = aa_log:get_text_message_from_packet(Oldpacket),  
      OutsideJ0 = jiffy:decode(erlang:list_to_binary(OJSON)),
      Oldofflinelist = ej:get({<<"offlinelist">>},OutsideJ0),
      {xmlel,<<"message">>,Attr,_} = Oldpacket,
      NewID = fxml:get_tag_attr_s(<<"id">>, Oldpacket),
      MsgTime = fxml:get_tag_attr_s(<<"msgTime">>, Packet),
      OLdid = fxml:get_tag_attr_s(<<"id">>, Packet),

      case ej:get({<<"mask">>},StructJson) of
        undefined ->
          Mask = <<"0">>;
        Mask ->
          skip
      end,
      At = ej:get({<<"at">>},StructJson),

      J1 = ej:delete({<<"groupid">>},StructJson),
      J2 = ej:delete({<<"groupname">>},J1),
      J3 = ej:delete({<<"groupmember">>},J2),
      J4 = ej:delete({<<"mask">>},J3),
      J5 = ej:delete({<<"at">>},J4),

      J6 = ej:set({<<"msgid">>},J5,OLdid),
      J7 = ej:set({<<"id">>},J6,OLdid),
      J8 = ej:set({<<"msgTime">>},J7,MsgTime),

      %%为客户端处理at信息（查看消息中是否有at 有的话提到最外层）
      if
        At =:= undefined ->
          OutsideJat = OutsideJ0;
        true ->
          #jid{user = Toid} = To,
          case (erlang:is_binary(At) =:= false) and lists:member(list_to_binary(Toid),At) of
            true->
              OutsideJat  = ej:set({<<"at">>},OutsideJ0, [list_to_binary(Toid)]);
            false->
              OutsideJat = OutsideJ0
          end
      end,

      OutsideJ1 = ej:set({<<"offlinelist">>},OutsideJat, Oldofflinelist++[J8]),
      OutsideJ2 = ej:set({<<"mask">>},OutsideJ1,Mask),
      OutsideJb = jiffy:encode(OutsideJ2),
      NewPacket = {xmlel,<<"message">>,Attr,[{xmlel,<<"body">>,[],[{xmlcdata,OutsideJb}]}]},
      OfflineZipKey =  <<NewID/binary, "/zip_offline_msg_body">>,
      {_,Ziplist} = lists:keyfind(OfflineZipKey, 1, Idlist),
      {lists:keyreplace(NewFrom, 1, Groupchat,{NewFrom,To,NewPacket}),lists:keyreplace(OfflineZipKey, 1, Idlist, {OfflineZipKey,[MsgId|Ziplist]})}
  end;


%% =================================================
%% 群组离线
%% =================================================

make_offline(super_groupchat,{SuperGroupchat,OldFrom,To,Packet},{Idlist,MsgId})->
  ?DEBUG("make offline .. super ~n\t SuperGroupchat = ~p",[SuperGroupchat]),
  %% old from with resource ,new without
  [JSON] = aa_log:get_text_message_from_packet(Packet),  
  StructJson = jiffy:decode(erlang:list_to_binary(JSON)),
  GroupidO = ej:get({<<"groupid">>},StructJson),
  Groupid = type_util:to_binary(GroupidO),
  #jid{user = Fromid,server = FDomain} = OldFrom ,
  From = #jid{user = Fromid,server = FDomain, resource = [], luser = Fromid, lserver = FDomain, lresource = []},
  % -------------------------------------------------
  % 判断压缩包内该用户是否是第一次出现
  % -------------------------------------------------
  case lists:keyfind(From, 1, SuperGroupchat) of
    false->
      {xmlel,<<"message">>,Attr,_} = Packet,
      MsgTime = fxml:get_tag_attr_s(<<"msgTime">>, Packet),
      OLdid = fxml:get_tag_attr_s(<<"id">>, Packet),
      {M,S,SS} = now(),
      NewID = erlang:integer_to_binary(M*1000000000000+S*1000000+SS),
      Attrs0 = lists:keydelete(<<"msgTime">>, 1, Attr),
      Attrs1 = lists:keydelete(<<"id">>, 1, Attrs0),
      Attrs2 = [{<<"id">>, NewID} | Attrs1],
      Groupname = ej:get({<<"groupname">>},StructJson),
      Groupimage = ej:get({<<"groupimage">>},StructJson),
      case ej:get({<<"mask">>},StructJson) of
        undefined ->
          Mask = <<"0">>;
        Mask ->
          skip
      end,

      At = ej:get({<<"at">>},StructJson),
      Atall = ej:get({<<"atall">>},StructJson),

      J1 = ej:delete({<<"groupid">>},StructJson),
      J2 = ej:delete({<<"groupname">>},J1),
      J3 = ej:delete({<<"groupimage">>},J2),
      J4 = ej:delete({<<"at">>},J3),
      J5 = ej:delete({<<"atall">>},J4),

      J6 = ej:set({<<"msgid">>},J5,OLdid),
      J7 = ej:set({<<"id">>},J6,OLdid),
      J8 = ej:set({<<"msgTime">>},J7,MsgTime),


      OutsideJ0 = {[]},
      %%为客户端处理at信息
      if
        At =:= undefined ->
          OutsideJat = OutsideJ0;
        true ->
          #jid{user = Toid} = To,
          case (erlang:is_binary(At) =:= false) and lists:member(list_to_binary(Toid),At) of
            true->
              OutsideJat  = ej:set({<<"at">>},OutsideJ0, [list_to_binary(Toid)]);
            false->
              OutsideJat = OutsideJ0
          end
      end,

      %%把atall 放到外面
       if
         Atall =:= undefined ->
             OutsideJatall = OutsideJat;
         true->
             OutsideJatall = ej:set({<<"atall">>},OutsideJat, Atall)
       end,
      
      OutsideJ1 = ej:set({<<"groupid">>},OutsideJatall, Groupid),
      OutsideJ2 = set_offline_Json_field(OutsideJ1, "groupname", Groupname),
      OutsideJ3 = set_offline_Json_field(OutsideJ2, "groupimage", Groupimage),
      
      OutsideJ4 = ej:set({<<"type">>},OutsideJ3, <<"10000">>),
      OutsideJ5 = ej:set({<<"mask">>},OutsideJ4, Mask),
      OutsideJ6 = ej:set({<<"offlinelist">>},OutsideJ5, [J8]),
      OutsideJb = jiffy:encode(OutsideJ6),
      
      NewPacket = {xmlel,<<"message">>,Attrs2,[{xmlel,<<"body">>,[],[{xmlcdata,OutsideJb}]}]},
      OfflineZipKey =  <<NewID/binary, "/zip_offline_msg_body">>,
      {[{From,To,NewPacket}|SuperGroupchat],[{OfflineZipKey,[MsgId]}|Idlist]};
    {_,_,Oldpacket}->
      [OJSON] = aa_log:get_text_message_from_packet(Oldpacket),  
      OutsideJ0 = jiffy:decode(erlang:list_to_binary(OJSON)),
      Oldofflinelist = ej:get({<<"offlinelist">>},OutsideJ0),

      {xmlel,<<"message">>,Attr,_} = Oldpacket,
      NewID = fxml:get_tag_attr_s(<<"id">>, Oldpacket),
      MsgTime = fxml:get_tag_attr_s(<<"msgTime">>, Packet),
      OLdid = fxml:get_tag_attr_s(<<"id">>, Packet),

      case ej:get({<<"mask">>},StructJson) of
        undefined ->
          Mask = <<"0">>;
        Mask ->
          skip
      end,
      At = ej:get({<<"at">>},StructJson),
      Atall = ej:get({<<"atall">>},StructJson),

      J1 = ej:delete({<<"groupid">>},StructJson),
      J2 = ej:delete({<<"groupname">>},J1),
      J3 = ej:delete({<<"groupimage">>},J2),
      J4 = ej:delete({<<"at">>},J3),
      J5 = ej:delete({<<"atall">>},J4),

      J6 = ej:set({<<"msgid">>},J5,OLdid),
      J7 = ej:set({<<"id">>},J6,OLdid),
      J8 = ej:set({<<"msgTime">>},J7,MsgTime),

      %%为客户端处理at信息（查看消息中是否有at 有的话提到最外层）
      if
        At =:= undefined ->
          OutsideJat = OutsideJ0;
        true ->
          #jid{user = Toid} = To,
          case (erlang:is_binary(At) =:= false) and lists:member(list_to_binary(Toid),At) of
            true->
              OutsideJat  = ej:set({<<"at">>},OutsideJ0, [list_to_binary(Toid)]);
            false->
              OutsideJat = OutsideJ0
          end
      end,

      %%把atall 放到外面
       if
         Atall =:= undefined ->
             OutsideJatall = OutsideJat;
         true->
             OutsideJatall = ej:set({<<"atall">>},OutsideJat, Atall)
       end,

      OutsideJ1 = ej:set({<<"offlinelist">>},OutsideJatall, Oldofflinelist++[J8]),
      OutsideJ2 = ej:set({<<"mask">>},OutsideJ1,Mask),
      OutsideJb = jiffy:encode(OutsideJ2),

      NewPacket = {xmlel,<<"message">>,Attr,[{xmlel,<<"body">>,[],[{xmlcdata,OutsideJb}]}]},
      OfflineZipKey =  <<NewID/binary, "/zip_offline_msg_body">>,
      {_,Ziplist} = lists:keyfind(OfflineZipKey, 1, Idlist),
      {lists:keyreplace(From, 1, SuperGroupchat,{From,To,NewPacket}),lists:keyreplace(OfflineZipKey, 1, Idlist, {OfflineZipKey,[MsgId|Ziplist]})}
  end.
 
set_offline_Json_field(StructJson,Type,SValue)->
  case SValue of
    undefined ->
      StructJson;
    _ ->
      ej:set({type_util:to_binary(Type)},StructJson, SValue)
  end.


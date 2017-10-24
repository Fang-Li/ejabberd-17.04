-module(aa_offline_mod).
-behaviour(gen_server).

-include("include/ejabberd.hrl").
-include("include/jlib.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include("include/logger.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([apns_push/4]).

%% 离线消息对象
%% -record(offline_msg, {us, timestamp, expire, from, to, packet}).
-define(NORMAL_EXPIRE,60*60*24*7).
-define(LITTLE_SECRETARY_EXPIRE,60*60*24*2).

%% ====================================================================
%% API functions
%% ====================================================================

-export([
   start_link/0,
   offline_message_hook_handler/3,
   offline_message_hook_handler/4,
   sm_register_connection_hook_handler/3,
   sm_remove_connection_hook_handler/3,
   user_available_hook_handler/1,
   make_offline/3,
   get_offline_msg/2
  ]).

-compile(export_all).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

sm_register_connection_hook_handler(_SID, JID, _Info) -> send_offline_msg(JID).

user_available_hook_handler(JID) -> send_offline_msg(JID).


%% =================================================
%% 过滤掉过期的msgid
%% =================================================

filter_out_expired_msg(User,KEY) ->
  %% 失效截止时间，从小到截止时间之间的msg删除掉
  {M,S,SS} = now(),
  Score = lists:sublist(erlang:integer_to_list(M*1000000000000+S*1000000+SS),1,13),
  %% 统一单位为毫秒,小秘书超时时间为2天，普通用户为7天
  case aa_hookhandler:is_little_secretary(User) of
    false ->
       ScoreDeadline = erlang:list_to_integer(Score) - ?NORMAL_EXPIRE * 1000;
    _ -> 
       ScoreDeadline = erlang:list_to_integer(Score) - ?LITTLE_SECRETARY_EXPIRE * 1000
  end,
  {ok,Count} = aa_hookhandler:ecache_cmd(["ZREMRANGEBYSCORE",KEY,"-inf",ScoreDeadline]),
  ?DEBUG("filter_out_expired_msg .. count = ~p",[Count]),
  Count.

send_offline_msg(#jid{user=User,server=Domain,resource=_Resource}=JID) ->
  %% JID={jid,"cc","test.com","Smack","cc","test.com","Smack"} 
  %% {jid,User,Domain,_,_,_,_} = JID,
  %% if user=1 and is not gropuchat then ... 
  KEY = <<User/binary, "@", Domain/binary, "/offline_msg">>,
  ?INFO_MSG("@@@@ send_offline_msg :::> KEY=~p >>>>>>>>>>>>>>>>>>>>>>>",[KEY]),
  %% R = gen_server:call(?MODULE,{range_offline_msg,KEY}),

  % -------------------------------------------------
  % 先过滤一次已经超时的key，减少对无效key的多次操作
  % -------------------------------------------------
  filter_out_expired_msg(User,KEY),
  {ok,R} = aa_hookhandler:ecache_cmd(["ZRANGE",KEY,"0","-1"]),
  %% TODO 这里，如果发送失败了，是需要重新发送的，但是先让他跑起来
  ?INFO_MSG("@@@@ send_offline_msg :::> KEY=~p ; R.size=~p~n",[KEY,length(R)]),
  sysn_send:offline_msg_send(JID, R),
%%   lists:foreach(fun(ID)->
%%     try  
%%       %% 如果是小秘书帐户，需要过滤hset中的key，如果有则不需要发送
%%       SEND = case User =:= "1" of
%%         true ->
%%           HK = User++"@"++Domain++"/"++Resource,
%%           case aa_hookhandler:ecache_cmd( ["HGET",HK,ID] ) of 
%%             {ok,undefined} ->
%%               true;
%%             _ ->
%%               false
%%           end;
%%         false ->
%%           true
%%       end,
%%       case SEND of 
%%         true ->      
%%             %% case gen_server:call(?MODULE,{ecache_cmd,["GET",ID]}) of
%%             case aa_hookhandler:ecache_cmd( ["GET",ID] ) of 
%%                 {ok,Obj} when erlang:is_binary(Obj) ->
%%                   {FF,TT,PP} = erlang:binary_to_term(Obj),
%%                   Rtn = case ejabberd_router:route(FF, TT, PP) of
%%                     ok -> ok; 
%%                     Err -> "Error: "++Err
%%                   end,
%%                   ?INFO_MSG("@ SEND :::::> KEY=~p; ID=~p ",[KEY,ID]);
%%                 Other ->  
%%                   %% ZREM_R = gen_server:call(?MODULE,{ecache_cmd,["ZREM",KEY,ID]}),
%%                   CMD = ["ZREM",KEY,ID],
%%                 ZREM_R = aa_hookhandler:ecache_cmd(CMD),
%%                   ?INFO_MSG("@ SEND [DEL]::::> KEY=~p; ID=~p; ERR=~p; ZREM_R=~p",[KEY,ID,Other,ZREM_R])  
%%             end;
%%         _ ->
%%           skip
%%       end
%%     catch
%%         E:I ->
%%             ?INFO_MSG("~p ; ~p",[E,I])  
%%     end,
%%     ok
%%     end,R),  
  ?INFO_MSG("@@@@ send_offline_message ::>KEY=~p  <<<<<<<<<<<<<<<<<",[KEY]),
  ok.


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
  ?INFO_MSG("group chat offline groupid .. ~p",[Groupid]),
  #jid{lserver = Domain} = From,
  Fromid = type_util:to_binary(Groupid),
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
      
      

sm_remove_connection_hook_handler(_SID, _JID, _Info) -> ok.

%% =================================================
%% 离线消息事件
%% 保存离线消息到 sorted set 中
%% 此时的score才是在redis中排序的关键，对于已有msgtime的仍保留
%% =================================================

offline_message_hook_handler(save,#jid{user=FromUser}= _From, #jid{user=User,server=Domain}=To, Packet) ->
  Type = fxml:get_tag_attr_s(<<"type">>, Packet),
  ID = fxml:get_tag_attr_s(<<"id">>, Packet),
  %% 单位为微秒
  {M,S,SS} = now(), 
  SCORE  =
    case  fxml:get_tag_attr_s(<<"msgTime">>, Packet) of
      <<>> ->
        %% 单位为毫秒
        lists:sublist(erlang:integer_to_list(M*1000000000000+S*1000000+SS),1,13);
      T->
        T
    end,
  IS_GROUP = aa_group_chat:is_group_chat(To),
  if IS_GROUP==false,FromUser=/= <<"messageack">>,User=/= <<"messageack">>, FromUser=/= <<"stanger_limit">>, User=/= <<"stanger_limit">>,Type=/= <<"error">>,Type=/= <<"groupchat">>,Type=/= <<"headline">> ->
      SYNCID = binary_to_list(<< ID/binary,"@",Domain/binary>>),
      %% Time = xml:get_tag_attr_s("msgTime", Packet),
      %% ?INFO_MSG("ERROR++++++++++++++++ Time=~p;~n~nPacket=~p",[Time,Packet]),
      %% {ok,TimeStamp} = getTime(Time),
      %% TODO 7天以后过期
      %% Exp = ?EXPIRE+TimeStamp,
      KEY = binary_to_list(<<User/binary, "@", Domain/binary, "/offline_msg">>),
      ?INFO_MSG(" save offline msg .. ~n\t type = ~p; ~n\t KEY = ~p",[Type,KEY]),
      %% gen_server:call(?MODULE,{store_offline_msg,KEY,SYNCID});
      %% 20141115: 防止因为排队产生瓶颈
      %% SCORE = integer_to_list(index_score()),
      CMD = ["ZADD",KEY,SCORE,SYNCID],
      aa_hookhandler:ecache_cmd(CMD);
     true ->
      ok
  end.

offline_message_hook_handler(#jid{user=_FromUser}=From, #jid{user=_User,server=Domain}=To, Packet) ->
    %% 当To是群组的时候，没有必要去推送，群组ID是没有token的
    case aa_group_chat:is_group_chat(To) of
      true -> 
          skip;
      _ -> 
          try
              ?INFO_MSG("offline_message_hook_handler ==> ",[]),
              case is_need_apns_push(Domain) of
                  true -> 
                      case get_apns_node(Domain) of
                          undefined ->
                              skip;
                          Apns_node ->
                              apns_push(From,To,Packet,Apns_node)
                      end;
                  _ ->
                      skip
              end 
          catch 
              C:E ->
                  Err = erlang:get_stacktrace(),
                  ?ERROR_MSG("offline_message_hook_error=> C=~p ; E=~p ; Err=~p",[C,E,Err])
          end,
          ok
    end.

offline_message_hook_handler(Args) ->
  ?INFO_MSG("ignored offline message .. ~p",[Args]),
  %% This parameter is returned to other same hook methods
  Args.

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, { ecache_node, ecache_mod=ecache_main, ecache_fun=cmd,apns_node }).

init([]) ->
  ?INFO_MSG("INIT_START_OFFLINE_MOD >>>>>>>>>>>>>>>>>>>>>>>> ~p",[liangchuan_debug]),  
  lists:foreach(
    fun(Host) ->
        ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, offline_message_hook_handler, 40),
        ejabberd_hooks:add(sm_remove_connection_hook, Host, ?MODULE, sm_remove_connection_hook_handler, 40),
        ejabberd_hooks:add(sm_register_connection_hook, Host, ?MODULE, sm_register_connection_hook_handler, 60),
        ejabberd_hooks:add(user_available_hook, Host, ?MODULE, user_available_hook_handler, 40)
    end, ?MYHOSTS),
  ?INFO_MSG("INIT_END_OFFLINE_MOD <<<<<<<<<<<<<<<<<<<<<<<<< ~p",[liangchuan_debug]),
  Conn = conn_ecache_node(),
  {ok,_,Node} = Conn,
  
  [Domain|_] = ?MYHOSTS,
    Apns_node = ejabberd_config:get_option({apns_node,Domain},fun(V)->V end),
    {ok, #state{ecache_node=Node,apns_node=Apns_node}}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_call({clear_offline_msg,KEY,ID},_From, State) -> 
  ?DEBUG("##### clear_offline_msg at ack :::> Key=~p ; ID=~p",[KEY,ID]),
  R = ecache_cmd(["ZREM",KEY,ID],State),
  {reply,R,State};
handle_call({range_offline_msg,KEY},_From, State) -> 
  %% 倒序: zrevrange
  %% 正序: zrange
  R = ecache_cmd(["ZRANGE",KEY,"0","-1"],State),
  ?DEBUG("##### range_offline_msg :::> Key=~p ; R=~p",[KEY,R]),
  {reply,R,State};
handle_call({store_offline_msg,KEY,ID},_From, State) -> 
  SCORE = integer_to_list(index_score()),
  ?DEBUG("##### store_offline_msg :::> Key=~p ; Time=~p ; ID=~p",[KEY,SCORE,ID]),
  R = ecache_cmd(["ZADD",KEY,SCORE,ID],State),
  ?DEBUG("##### store_offline_msg :::> R=~p",[R]),
  {reply,R,State};
handle_call({ecache_cmd,Cmd},_From, State) -> 
  ?DEBUG("##### ecache_cmd_on_offline_mod :::> Cmd=~p",[Cmd]),
  {reply,ecache_cmd(Cmd,State),State}.

ecache_cmd(Cmd,#state{ecache_node=Node,ecache_mod=Mod,ecache_fun=Fun}=_State) ->
  {ok,R} = rpc:call(Node,Mod,Fun,[Cmd]),
  %% ?DEBUG("==== ecache_cmd ===> Cmd=~p ; R=~p",[Cmd,R]),
  R.

handle_info(_Info, State) -> {noreply, State}.
terminate(Reason, State) -> 
  ?ERROR_MSG("offline mod terminate .. ~n\t Reason = ~p, ~n\t State = ~p",[Reason,State]),
  ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
%% ====================================================================
%% Internal functions
%% ====================================================================
timestamp() ->  
  {M, S, _} = os:timestamp(),
  M * 1000000 + S.
getTime(Time) when is_binary(Time) ->
  {ok,erlang:binary_to_integer(Time)};
getTime(Time) when is_list(Time) ->
  {ok,erlang:list_to_integer(Time)};
getTime([]) ->
  {ok,timestamp()}.
index_score()-> {M,S,T} = now(),  M*1000000000000+S*1000000+T.
conn_ecache_node() ->
  try
    [Domain|_] = ?MYHOSTS, 
    N = ejabberd_config:get_option({ecache_node,Domain},fun(V)->V end),
    {ok,net_adm:ping(N),N}
  catch
    E:I ->
      Err = erlang:get_stacktrace(),
      log4erl:error("error ::::> E=~p ; I=~p~n Error=~p",[E,I,Err]),
      {error,E,I}
  end.

get_apns_node(TServer) ->
    _Apns_node = ejabberd_config:get_option({apns_node,get_real_domain(TServer)},fun(V)->V end).

is_need_apns_push(TServer) ->
    case get_real_domain(TServer) of
        "yiqibo.tv" ->
            true;
        "yuejian.net" ->
            true;
        "17lfyq.com" ->
            true;
        "youxi01.cn" ->
            true;
        "yiyuread.com" ->
            true;
        _ ->
            skip
    end.


get_real_domain(TServer) ->
    [Head|Domain] = string:tokens(TServer,"."),
    case Head of
        "super_group" ->
            string:join(Domain,".");
        "group" -> 
            string:join(Domain,".");
        _ ->
            TServer
    end.
apns_push(#jid{user=FU,server=FS,resource=FR}=From,#jid{user=TU,server=TS,resource=TR}=To,Packet,Node)->
  ?INFO_MSG("apns_push ::::> ~nFrom=~p ; ~nTo=~p ; ~nNode=~p ; ~nPacket=~p",[From,To,Node,Packet]),
  F = case is_list(FR) of
    true ->
      case length(FR)>0 of
        true ->
          FU++"@"++FS++"/"++FR;
        _ ->
          FU++"@"++FS
      end;
    false ->
      FU++"@"++FS
  end,
  T = case is_list(TR) of
    true ->
      case length(TR)>0 of
        true ->
          TU++"@"++TS++"/"++TR;
        _ ->
          TU++"@"++TS
      end;
    false ->
      TU++"@"++TS
  end,

  [JSON] = aa_log:get_text_message_from_packet(Packet),
  BJson = list_to_binary(JSON),
  StructJson = jiffy:decode(BJson),

  case Packet of 
    {xmlel,<<"message">>,Attr,_} -> 
      D = dict:from_list(Attr),
      ID      = case dict:is_key(<<"id">>,D) of true-> dict:fetch(<<"id">>,D); false-> "" end,
      %% From    = case dict:is_key("from",D) of true-> dict:fetch("from",D); false-> "" end,
      %% To      = case dict:is_key("to",D) of true-> dict:fetch("to",D); false-> "" end,
      MsgType = case dict:is_key("msgtype",D) of true-> dict:fetch("msgtype",D); false-> "" end,
      Mask = type_util:to_list(ej:get({<<"mask">>}, StructJson, "0")),
      MaskAll = type_util:to_list(ej:get({<<"mask_all">>}, StructJson, "0")),

      %% #jid{user=User,server=Domain} = jlib:string_to_jid(xml:get_tag_attr_s("to", Packet)),
      #jid{user=User,server=Domain} = To, 
      KEY = User++"@"++Domain++"/offline_msg",
      %% R = gen_server:call(?MODULE,{range_offline_msg,KEY}),
      {ok,Msgidlist} = aa_hookhandler:ecache_cmd(["ZRANGE",KEY,"0","-1"]),
      %% {ok,<<"none">>}
      %% 20141211 : 修正过滤掉无效的 key 
      %% 20141223 : 这里过滤是画蛇添足，没有实际意义只会增加服务器负担
      %% R0 = lists:map(fun(K0)->
      %%   case catch aa_hookhandler:ecache_cmd(["TYPE",K0]) of
      %%     {ok,<<"none">>} ->
      %%       skip;
      %%     Obj0 ->
      %%       Obj0
      %%   end
      %% end,R),
      %% R1 = [ X0 || X0 <- R0 , X0 =/= skip ],
      %% B = length(R1),
      B  = get_no_hide_msg_num(0,Msgidlist),
      case MsgType of
        <<"msgStatus">> ->
          ?DEBUG("apns_push_skip msgtype=msgStatus ; id=~p",[ID]),
          skip;
        _ ->
          case (Mask == "0") and (MaskAll == "0") of
            true ->
              case net_adm:ping(Node) of
                pang ->
                  ?INFO_MSG("push_apn_by_log_pang ::::> ~p",[Packet]);
                pong ->
                  Message = {apns_push,ID,F,T,MsgType,StructJson,B,BJson},
                  %% 150110: get_mask_user接口有些变动,增加一个push参数，如果这个参数为1就推送否则不推  
                  %% 需求来自 戚银
                  %% only for msgtype=normalchat 
                  case MsgType =:= "normalchat" of 
                    true ->
                      case ej:get({<<"push">>},StructJson) of 
                        <<"1">> ->
                          ?INFO_MSG("push_apn_by_log_pong ::::> ~p",[Message]),
                          %% case lists:member(TU,["1680028","29"]) of
                          %%   true ->
                          %%     ?ERROR_MSG("apns_push ::::> ~nFrom=~p ; ~nTo=~p ; ~nNode=~p ; ~nPacket=~p",[From,To,Node,Packet]);
                          %%   _ ->
                          %%     nop
                          %% end,
                          aa_ios_provider_send:send(Node,Message);
                        Other ->
                          ?INFO_MSG("[v.150110] apns_push_skip push=~p ; id=~p",[Other,ID]) 
                      end;
                    _ ->
                      ?INFO_MSG("push_apn_by_log_pong ::::> ~p",[Message]),
                      case lists:member(TU,["1683529","1687142"]) of
                        true ->
                          ?ERROR_MSG("apns_push ::::> ~nFrom=~p ; ~nTo=~p ; ~nNode=~p ; ~nPacket=~p",[From,To,Node,Packet]);
                        _ ->
                          nop
                      end,
                      aa_ios_provider_send:send(Node,Message) 
                  end
              end;
            _ ->
              ?DEBUG("apns_push_skip mask=1 ; id=~p",[ID]),
              skip
          end
      end;
    _ ->
      ?DEBUG("apns_push_skip not_message",[]),
      skip
  end.

set_offline_Json_field(StructJson,Type,SValue)->
  case SValue of
    undefined ->
      StructJson;
    _ ->
      ej:set({type_util:to_binary(Type)},StructJson, SValue)
  end.

%%过滤隐式消息 社交圈动态消息
get_no_hide_msg_num(N,[])->
  N;
get_no_hide_msg_num(N,R)->
  [H|L] = R,
  <<SubStr:9/binary,_RestId/binary>> = H,
  if
    SubStr =:= <<"hide_msg_">> ->
      get_no_hide_msg_num(N,L);
    true->
      get_no_hide_msg_num(N+1,L)
  end.

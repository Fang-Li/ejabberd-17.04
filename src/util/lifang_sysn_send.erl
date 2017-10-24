% -------------------------------------------------
% 离线消息流程
% -------------------------------------------------
-module(lifang_sysn_send).
-compile(export_all).
-include("include/ejabberd.hrl").
-include("include/jlib.hrl").
-include("include/logger.hrl").

%% =================================================
%% 完整jid
%% =================================================

jid(User) ->
   jlib:make_jid(User,<<"yuejian.net">>,<<>>).
jid(User,Domain) ->
   jlib:make_jid(User,Domain,<<>>).


%% =================================================
%% 登陆
%% =================================================
user_available_hook_handler(JID) -> send_offline_msg(JID).

display(Args) ->
  ok.


%% =================================================
%% 发送离线消息
%% =================================================

send_offline_msg(#jid{user=User,server=Domain,resource=_Resource}=JID) ->
  KEY = <<User/binary,"@",Domain/binary,"/offline_msg">>,
  {ok,R} = aa_hookhandler:ecache_cmd(["ZRANGE",KEY,"0","-1"]),
  do_offline_send(JID, R).
  

%% =================================================
%% 打包离线消息
%% =================================================

do_offline_send(Jid, R) ->
  deel_offline_msg_send(Jid,R,100,{[],[],[],[],[]}).

deel_offline_msg_send(_,[],_,OfflineMsg)->
  {Normalchat,GroupChat,SuperGroupChat,System,Idlist} = OfflineMsg,
  lists:foreach(fun({Key,Ziplist})->
               Cmd = ["SETEX",Key,integer_to_list(30),erlang:term_to_binary(Ziplist)],
                aa_hookhandler:ecache_cmd(Cmd)
              end, Idlist),
  display([empty,Normalchat,GroupChat,SuperGroupChat,System]),
  offline_route(Normalchat),
  offline_route(GroupChat),
  offline_route(SuperGroupChat),
  offline_route(System);
  
deel_offline_msg_send(Jid,R,0,OfflineMsg)->
  {Normalchat,GroupChat,SuperGroupChat,System,Idlist} = OfflineMsg,
  lists:foreach(fun({Key,Ziplist})->
               Cmd = ["SETEX",Key,integer_to_list(30),erlang:term_to_binary(Ziplist)],
                aa_hookhandler:ecache_cmd(Cmd)
              end, Idlist),
  display([notempty,Normalchat,GroupChat,SuperGroupChat,System]),
  offline_route(Normalchat),
  offline_route(GroupChat),
  offline_route(SuperGroupChat),
  offline_route(System),
  deel_offline_msg_send(Jid,R,200,{[],[],[],[],[]});
  
deel_offline_msg_send(#jid{lresource = Resource}  = Jid,R,N,OfflineMsg)->
  [Id|LR] = R,
 %% ?WARNING_MSG("----------R ~p",[R]),
  {NewOfflineMsg,NewNum} =
    try
      {NormalChatoffline,Groupchat,SuperGroupChat,Systemchatoffline,Idlist} = OfflineMsg,
      case lifang_offline_mod:get_offline_msg(Jid, Id) of
        {From,To,Packet}->
           case fxml:get_tag_attr_s(<<"msgtype">>, Packet) of 
             <<"normalchat">> ->
               {New_NormalChatoffline,ZipIdList} = lifang_offline_mod:make_offline(normalchat,{NormalChatoffline,From,To,Packet},{Idlist,Id}),
               {{New_NormalChatoffline,Groupchat,SuperGroupChat,Systemchatoffline,ZipIdList},N-1};
             <<"groupchat">> ->
               {New_GroupChatoffline,ZipIdList} = lifang_offline_mod:make_offline(groupchat,{Groupchat,From,To,Packet},{Idlist,Id}),
               {{NormalChatoffline,New_GroupChatoffline,SuperGroupChat,Systemchatoffline,ZipIdList},N-1};
             <<"super_groupchat">> ->
               {New_SuperGroup,ZipIdList} = lifang_offline_mod:make_offline(super_groupchat,{SuperGroupChat,From,To,Packet},{Idlist,Id}),
               {{NormalChatoffline,Groupchat,New_SuperGroup,Systemchatoffline,ZipIdList},N-1};
             _->
               {{NormalChatoffline,Groupchat,SuperGroupChat,[{From,To,Packet}|Systemchatoffline],Idlist},0}
           end;
        _R->
          ?ERROR_MSG("--------------error ~p",[_R]),
          {OfflineMsg,N-1}
      end
    catch
      E:R->
        ?ERROR_MSG("deel_offline_msg_send~p",[{E,R,erlang:get_stacktrace()}]),
        {OfflineMsg,N-1}
    end,
  deel_offline_msg_send(Jid,LR,NewNum,NewOfflineMsg).

offline_route(PacketList)->
  lists:foldr(fun({From,To,Packet},Acc)->
                #jid{server=Domain2}=To,
                %% User2="15538",
                User2= <<"15538">>,
                To2 = jlib:make_jid({User2,Domain2,<<"">>}),
                #jid{lresource = Resource} = To,
                timer:sleep(500),
                ejabberd_router:route(From,To2,Packet),
                Acc
              end, ok, PacketList).
            

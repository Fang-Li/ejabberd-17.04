-module(lifang_sysn_send_pro).
-compile(export_all).
-include("include/ejabberd.hrl").
-include("include/jlib.hrl").
-include("include/logger.hrl").

-define(TU,1697379).

jid(User) ->
  jid(User,?TU,"yiqibo.tv").
jid(User,Domain) when is_list(Domain) ->
  jid(User,?TU,Domain);
jid(User,Tu) when is_integer(Tu) ->
  jid(User,Tu,"yiqibo.tv").
jid(User,TU,Domain) ->
  put(lifang_offline_to,integer_to_list(TU)),
  user_available_hook_handler(#jid{user=integer_to_list(User),server=Domain}).

  
user_available_hook_handler(JID) -> send_offline_msg(JID).

display(Args) ->
  ok.

send_offline_msg(#jid{user=User,server=Domain,resource=_Resource}=JID) ->
  KEY = User++"@"++Domain++"/offline_msg",
  {ok,R} = aa_hookhandler:ecache_cmd(["ZRANGE",KEY,"0","-1"]),
  offline_msg_send(JID, R).
  
offline_msg_send(Jid, R) ->
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
  timer:sleep(5000),
  offline_route(SuperGroupChat),
  timer:sleep(5000),
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
         %% ?WARNING_MSG("------aa_offline_mod 1 ~p",[{From, To, Packet}]),
          case check_version(Resource) of      %%新老版本兼容
            new->
              case xml:get_tag_attr_s("msgtype", Packet) of 
                "normalchat"->
                  {New_NormalChatoffline,ZipIdList} = lifang_offline_mod:make_offline(normalchat,{NormalChatoffline,From,To,Packet},{Idlist,Id}),
                  {{New_NormalChatoffline,Groupchat,SuperGroupChat,Systemchatoffline,ZipIdList},N-1};
                "groupchat"->
                  {New_GroupChatoffline,ZipIdList} = lifang_offline_mod:make_offline(groupchat,{Groupchat,From,To,Packet},{Idlist,Id}),
                  {{NormalChatoffline,New_GroupChatoffline,SuperGroupChat,Systemchatoffline,ZipIdList},N-1};
                "super_groupchat"->
                  {New_SuperGroup,ZipIdList} = lifang_offline_mod:make_offline(super_groupchat,{SuperGroupChat,From,To,Packet},{Idlist,Id}),
                  {{NormalChatoffline,Groupchat,New_SuperGroup,Systemchatoffline,ZipIdList},N-1};
                _->
                  {{NormalChatoffline,Groupchat,SuperGroupChat,[{From,To,Packet}|Systemchatoffline],Idlist},0}
              end;
            old->
              {{NormalChatoffline,Groupchat,SuperGroupChat,[{From,To,Packet}|Systemchatoffline],Idlist},N-1}
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
              %% User2="1697379",
              case get(lifang_offline_to) of
                undefined -> User2 = ?TU;
                User2 -> User2
              end,
              To2 = {jid,User2,Domain2,"",User2,Domain2,""},
              #jid{lresource = Resource} = To,
              case check_version(Resource) of
                old->
                  ejabberd_router:route(From,To2,Packet);
                new->
                  ejabberd_router:route(From,To2,Packet)
              end,
              Acc
            end, ok, PacketList).
            
%%检查离线消息的新老版本
check_version(Resource)->
  case Resource of 
    ""->
      old;
    undefined ->
      new;    
    _->
      case string:sub_string(Resource, 1, 6) of
        "app_v2"->
          R = string:sub_string(Resource, 1, 10),
          if
            R >= "app_v2.1.2"->
              new;
            true->
              old
          end;
        _->
          old
      end
  end.


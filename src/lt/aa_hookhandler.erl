-module(aa_hookhandler).
-behaviour(gen_server).

-include("include/ejabberd.hrl").
-include("include/jlib.hrl").


-define(HTTP_HEAD,"application/x-www-form-urlencoded").
-define(TIME_OUT,1000*15).

-define(NORMAL_EXPIRE,60*60*24*7).
-define(LITTLE_SECRETARY_EXPIRE,60*60*24*2).
-include("include/logger.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,ecache_cmd/1,mysql_cmd/1]).
%% -export([handle_call/2, handle_cast/1]).

-compile(export_all).

%% ====================================================================
%% API functions
%% ====================================================================

-export([
   start_link/0,
   user_send_packet_handler/3
]).

%% -record(state, { ecache_node, ecache_mod=ecache_main, ecache_fun=cmd log_node }).
-record(state, {}). 
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%user_send_packet(From, To, Packet) -> ok
user_send_packet_handler(#jid{user=FUser,server=FDomain}=From, #jid{user=TUser,server=TDomain}=To, Packet) ->
  try
      case Packet of 
        {_,<<"message">>,Attr,_} ->
          D = dict:from_list(Attr),
          MT = case dict:is_key(<<"msgtype">>,D) of true-> dict:fetch(<<"msgtype">>,D); _-> "" end,
        case MT of
          <<"msgStatus">> ->
            %% gen_server:cast(?MODULE,{group_chat_filter,From,To,Packet}),
            %% gen_server:cast(aa_log,{store,Packet});
            %% 20141223 : 为了提高效率，这个地方必须放弃 otp
            handle_cast({group_chat_filter,From,To,Packet},#state{});
          _ ->
            ?INFO_MSG("###### my_hookhandler ::::> user_send_packet_handler ~n ~p",[liangchuan_debug]),
            BlacklistKey = {jlib:jid_to_string({FUser,FDomain,""}),jlib:jid_to_string({TUser,TDomain,""})}, 

            ?DEBUG("BlacklistKey=~p",[BlacklistKey]),
            BlacklistCheck = aa_blacklist2:check(From, To) =/= true,
            BlockedCheck = aa_shielding_system:check_is_blocked(From,To,Packet) =/= true,
            StrangerLimitCheck = aa_stranger_limit:check_deel_stranger_limit(From, To, Packet,nosend),
            % -------------------------------------------------
            % 满足黑名单和免打扰和陌生人条数限制后发送消息
            % -------------------------------------------------
            case BlacklistCheck  and BlockedCheck  and StrangerLimitCheck of
              true ->
                %% gen_server:cast(?MODULE,{group_chat_filter,From,To,Packet}),
                %% gen_server:cast(aa_log,{store,Packet});
                %% 20141223 : 为了提高效率，这个地方必须放弃 otp
                aa_log:calculat_msg(),
                handle_cast({group_chat_filter,From,To,Packet},#state{});
              _ ->
                %% gen_server:cast(?MODULE,{server_ack,From,To,Packet}),
                %% 20141223 : 为了提高效率，这个地方必须放弃 otp
                handle_cast({server_ack,From,To,Packet},#state{}),
                MSG_ID = fxml:get_tag_attr_s(<<"id">>, Packet),
                ?INFO_MSG("discard_message ::> Blacklist_key=~p ; MSG_ID=~p ,check:{~p}",[BlacklistCheck,MSG_ID, {BlacklistKey, BlockedCheck, StrangerLimitCheck}])
            end 
        end;
      _ ->
        skip
    end 
  catch 
    _:_->
      Err = erlang:get_stacktrace(),
      ?ERROR_MSG("user_send_packet_handler_error ~p",[Err])
  end,
  ok.
  
user_send_packet_handler(Args) ->
  ?INFO_MSG("packet send handler .. ignore packet ~p",[Args]),
  case Args of
    {#xmlel{name = <<"message">>} = El, #message{from= From,to = To} = _Pkt, _State} ->
       user_send_packet_handler(From, To, El);
    _ ->
      ignore
  end,
  Args.

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(dmsg,{mid,pid}).

init([]) ->
  ?INFO_MSG("INIT_START >>>> ~p",[liangchuan_debug]),  
  lists:foreach(
    fun(Host) ->
    ?INFO_MSG("#### _begin Host=~p~n",[Host]),
    ejabberd_hooks:add(user_send_packet,Host,?MODULE, user_send_packet_handler ,70),
        ?INFO_MSG("#### user_send_packet Host=~p~n",[Host])
      end, ?MYHOSTS),
  ?INFO_MSG("INIT_END <<<< ~p",[liangchuan_debug]),
  Conn = conn_ecache_node(),
  ?INFO_MSG("INIT_END <<<<<<<<<<<<<<<<<<<<<<<<< Conn=~p",[Conn]),
  %% {ok,_,Node} = Conn,
  mnesia:create_table(dmsg,[{attributes,record_info(fields,dmsg)},{ram_copies,[node()]}]),
  %% {ok, #state{ecache_node=Node}}.
  %% 20141223 : ecache_node 不再存储在 state 中, 瓶颈
  {ok, #state{}}.



%% =================================================
%% 20141223 : 其实这两个方法已经被废弃了,用更高效的方式实现了
%% =================================================

handle_call({ecache_cmd,Cmd}, _F, State) ->
  ?DEBUG("==== ecache_cmd ===> Cmd=~p",[Cmd]),
  R = rpc:call(ecache_node(),ecache_main,cmd,[Cmd]),
  {reply, R, State};
handle_call({sync_packet,K,From,To,Packet}, _F, State) ->
  {M,S,SS} = now(), 
  MsgTime = lists:sublist(erlang:integer_to_list(M*1000000000000+S*1000000+SS),1,13),
        {Tag,E,Attr,Body} = Packet,
        RAttr0 = lists:map(fun({K2,V})-> case K2 of "msgTime" -> skip; _-> {K2,V} end end,Attr),
        RAttr1 = lists:append([X||X<-RAttr0,X=/=skip],[{"msgTime",MsgTime}]),
        RPacket = {Tag,E,RAttr1,Body},
        V = term_to_binary({From,To,RPacket}),
        ?DEBUG("==== sync_packet ===> insert K=~p~nV=~p",[K,V]),
        Cmd = ["PSETEX",K,integer_to_list(1000*60*60*24*7),V],
    R = rpc:call(ecache_node(),ecache_main,cmd,[Cmd]),
        aa_offline_mod:offline_message_hook_handler(save,From,To,RPacket),
        {reply, R, State}.


handle_cast({group_chat_filter,From,To,Packet}, State) ->
  try
    filter_cast({From,To,Packet,true}, State) 
  catch 
    _:_ ->
      Err = erlang:get_stacktrace(),
      ?ERROR_MSG("group_chat_filter_error ~p",[Err])
  end,
  {noreply, State};
handle_cast({group_chat_filter,From,To,Packet,SACK}, State) ->
  try
    filter_cast({From,To,Packet,SACK}, State) 
  catch 
    _:_ ->
      Err = erlang:get_stacktrace(),
      ?ERROR_MSG("group_chat_filter_error ~p",[Err])
  end,
  {noreply, State};
handle_cast({server_ack,_From,_To,_Packet}, State) ->
  %% server_ack(_From,_To,_Packet,State).
  {noreply, State};
handle_cast(_, State) -> {noreply, State}.

filter_cast({#jid{server=Domain,user = _FUser }=From,#jid{user=TUser,server=TDomain}=To,Packet,SACK}, State) ->
  case Packet of 
    {_,<<"message">>,_Attrs,_Child} -> 
      case SACK of true -> server_ack(From,To,Packet,State); _-> skip end,
      case aa_group_chat:is_group_chat(To) of 
        true ->
          ?DEBUG("###### send_group_chat_msg ###### From=~p ; Domain=~p",[From,Domain]),
          {_,<<"message">>,Attr,_} = Packet,
          D = dict:from_list(Attr),
          MsgType = case dict:is_key(<<"msgtype">>,D) of true-> dict:fetch(<<"msgtype">>,D); _-> "" end,
          case MsgType of
            <<"msgStatus">> ->
               ?DEBUG("###### ack_group_chat_msg ###### Packet=~p",[Packet]),
               message_handler(From,To,Packet,State);
            <<"system">> ->
               handle_system_message(From,To,Packet);
            _ ->
              case TUser of
                <<"0">> ->
                  skip;
                _ ->
                  case aa_group_chat:check_group_type(TDomain) of
                    <<"group">>->
                      aa_group_chat:route_group_msg(From,To,Packet);
                    <<"super_group">> ->
                      aa_super_group_chat:route_group_msg(From, To, Packet);
                    _->
                      skip
                  end
              end 
          end;
        false ->
          message_handler(From,To,Packet,State)
      end;
    _ ->
      skip
  end,
  {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
%% ====================================================================
%% Internal functions
%% ====================================================================
ecache_node()->
  {ok,_,Node} = conn_ecache_node(),
  Node.
conn_ecache_node() ->
  try
    [Domain|_] = ?MYHOSTS,
    N = ejabberd_config:get_option({ecache_node,Domain},fun(V)->V end),
    {ok,net_adm:ping(N),N}
  catch
    E:I ->
    Err = erlang:get_stacktrace(),
    ?ERROR_MSG("error ::::> E=~p ; I=~p~n Error=~p",[E,I,Err]),
    {error,E,I}
  end.

server_ack(#jid{user=_FU,server=_FD}=_From,_To,_Packet,_State) ->
  ok.
server_ack(old,#jid{user=_FU,server=FD}=_From,_To,Packet,_State) ->
  Domain = FD,
        {_,<<"message">>,Attr,_} = Packet,
        D = dict:from_list(Attr),
        _T = dict:fetch(<<"type">>, D),
        MT = case dict:is_key(<<"msgtype">>,D) of true-> dict:fetch(<<"msgtype">>,D); _-> <<"">> end,
        SRC_ID_STR = case dict:is_key(<<"id">>, D) of true -> dict:fetch(<<"id">>, D); _ -> <<"">> end,
        ACK_FROM = case ejabberd_config:get_option({ack_from ,Domain},fun(V)->V end) of true -> true; _ -> false end,
        if ACK_FROM and ( ( MT=:= <<"normalchat">> ) or ( MT=:= <<"groupchat">>) or (MT=:= <<"super_groupchat">>) ) ->
           case dict:is_key(<<"from">>, D) of
               true ->
                  {M8,S8,T8} = now(),
                  ID8 = list_to_binary(integer_to_list(M8*1000000000000+S8*1000000+T8)),
                   Attributes = [
                           {<<"id">>,ID8},
                           {<<"to">>,dict:fetch(<<"from">>, D)},
                           {<<"from">>,<<"messageack@",Domain/binary>>},
                           {<<"type">>,<<"normal">>},
                           {<<"msgtype">>,<<"">>},
                           {<<"action">>,<<"ack">>}
                   ],
                   Child = [{xmlel, <<"body">>, [], [
                                   {xmlcdata, <<"{'src_id':'",SRC_ID_STR/binary,"','received':'true'}">>}
                   ]}],
                   Answer = {xmlel,<<"message">>, Attributes , Child},
                   FF = jlib:string_to_jid(fxml:get_tag_attr_s(<<"from">>, Answer)),
                   TT = jlib:string_to_jid(fxml:get_tag_attr_s(<<"to">>, Answer)),
                   ?DEBUG("Answer ::::> FF=~p ; TT=~p ; P=~p ", [FF,TT,Answer] ),
                   case catch ejabberd_router:route(FF, TT, Answer) of
                           ok -> ?DEBUG("Answer ::::> ~p ", [ok] );
                           _ERROR -> ?DEBUG("Answer ::::> error=~p ", [_ERROR] )
                   end,
                   answer;
               _ ->
                   ?DEBUG("~p", [skip_01] ),
                   skip
           end;
          true ->
                ok
        end.


      
message_handler(#jid{user=FU,server=FD,resource=_FR}=From,#jid{user=TU,server=TD}=To,Packet,State) ->
  %% TODO 特殊处理
  %% <message id="xxxxx" from="yy@group.test.com" to="123456@test.com" type="normal" msgtype=“system”>
  %% TODO 处理 message 消息，进来的都是 message
  {_,<<"message">>,Attr,_} = Packet, 
  ?DEBUG("Attr=~p", [Attr] ), 
  D = dict:from_list(Attr), 
  T = dict:fetch(<<"type">>, D), 
  MT = case dict:is_key(<<"msgtype">>,D) of true-> dict:fetch(<<"msgtype">>,D); _-> <<"">> end,
  %%  SRC_ID_STR = case dict:is_key("id", D) of true -> dict:fetch("id", D); _ -> "" end,
  %%  ?DEBUG("SRC_ID_STR=~p ; other=~p", [SRC_ID_STR,{From,To,Domain}] ),
  %ACK_FROM = case ejabberd_config:get_local_option({ack_from ,Domain}) of true -> true; _ -> false end,
  ACK_FROM = true,  
  ?DEBUG("ack_from=~p ; Domain=~p ; T=~p ; MT=~p",[ACK_FROM,{FD,TD},T,MT]),
  % -------------------------------------------------
  % 隐藏消息和消息回执和小秘书离线消息处理
  % -------------------------------------------------
  Mid = fxml:get_attr_s(<<"id">>, Attr),
  if ACK_FROM,MT=/=[],MT=/= <<"msgStatus">>,FU=/= <<"messageack">>,FU=/= <<"stanger_limit">> -> 
       %% ?DEBUG("==> SyncRes = handle_call({sync_packet,SYNCID,From,To,Packet},[],State), 
       %% 20141115 : 防止在本模块排队产生瓶颈
       sync_hide(From,To,Packet,[Mid]);
       %%  ?DEBUG("==> SYNC_RES new => ~p ; ID=~p",[SyncRes,SRC_ID_STR]);
       %% ack_task({new,SYNCID,From,To,Packet}); 
     ACK_FROM,MT=:=<<"msgStatus">>,TU=/= <<"stranger_limit">> -> 
       handle_receipt_message(From,[Mid,State]);
     true -> 
       skip 
  end, 
  ok.

ack_sync( SYNCID,State , N ) when N =< 1 ->
  Cmd = ["DEL",SYNCID],
  case rpc:call(ecache_node(),ecache_main,cmd,[Cmd]) of  
    <<"0">> ->
      timer:sleep(200),
      <<IsHide:9/binary,RestId/binary>> = SYNCID,
      case IsHide of
        <<"hide_msg_">> ->
          ?WARNING_MSG("HIDE ACK_ID=~p ; N=~p",[SYNCID,N]),
          Cmd1 = ["DEL", RestId],
          rpc:call(ecache_node(),ecache_main,cmd,[Cmd1]),
          skip;
        _->
          ?WARNING_MSG("REDEL ACK_ID=~p ; N=~p",[SYNCID,N]),
          ack_sync(SYNCID,State,N+1)
      end;
    _ ->
      ok
  end;
ack_sync(SYNCID,_State,N) when N > 1 ->
  ?WARNING_MSG("SYNC_RES ack_id_notfound ; ACK_ID=~p",[SYNCID]).
      
  

  
ack_task({new,ID,From,To,Packet})-> 
  TPid = erlang:spawn(fun()-> ack_task(ID,From,To,Packet) end), 
  mnesia:dirty_write(dmsg,#dmsg{mid=ID,pid=TPid});
ack_task({ack,ID})->
  ack_task({do,ack,ID});
ack_task({offline,ID})-> 
  ack_task({do,offline,ID});
ack_task({do,M,ID})-> 
  try [{_,_,ResendPid}] = mnesia:dirty_read(dmsg,ID), ResendPid!M catch _:_-> ack_err end.
ack_task(ID,_From,_To,Packet)-> 
  ?INFO_MSG("ACK_TASK_~p ::::> START.",[ID]),
  receive 
    offline-> 
      mnesia:dirty_delete(dmsg,ID), 
      ?INFO_MSG("ACK_TASK_~p ::::> OFFLINE.",[ID]); 
    ack -> 
      mnesia:dirty_delete(dmsg,ID), 
      ?INFO_MSG("ACK_TASK_~p ::::> ACK.",[ID]) 
  after ?TIME_OUT -> 
    ?INFO_MSG("ACK_TASK_~p ::::> AFTER",[ID]), 
    mnesia:dirty_delete(dmsg,ID), 
    %% aa_offline_mod:offline_message_hook_handler(From,To,Packet),
    {xmlel,<<"message">>,Header,_ } = Packet,
    D = dict:from_list(Header),
    V = dict:fetch("msgtype", D),
    case V of <<"msgStatus">> -> ok;
      _ -> ack_task({offline,ID})  
    end 
  end.  


route_3(#jid{server=_FromDomain}=From,#jid{user=User,server=Server}=To,Packet,J4B)->
  StructJson = jiffy:decode(J4B),
  %  JO = aa_super_group_chat:set_super_group_msgsource(FromDomain, Obj),
  %  NewJ = type_util:to_binary(rfc4627:encode(StructJson)), 
  {X,E,Attr,_} = Packet,
  RAttr0 = lists:map(fun({K,V})-> 
    case K of 
      <<"id">> ->
        %%这个消息为隐藏消息 所以加个标识，在推送那里做过滤
        {A,B,C} = now(),
        UUID = list_to_binary(integer_to_list(A)++integer_to_list(B)++integer_to_list(C)),
        case ej:get({<<"type">>},StructJson) of
          <<"20">> ->
            {K,<<"hide_msg_",UUID/binary>>};
          <<"19">> ->
            {K,<<"hide_msg_",UUID/binary>>};
          _->
            case ej:get({<<"mask">>},StructJson) of
              <<"1">> ->
                {K,<<"hide_msg_",UUID/binary>>};
              _->
                {K,UUID}
            end
        end;
      <<"to">> -> {K,<<User/binary,"@",Server/binary>>};
      _-> {K,V} 
    end 
  end,Attr),
  Body = [{xmlel,<<"body">>,[],[{xmlcdata,J4B}]}],
  RPacket = {X,E,RAttr0,Body},
  ?DEBUG("route_3 :::> ~n\tpacket=~p",[RPacket]),
  %% 20141223 : 不管发成发不成，都得存起来
  %% gen_server:cast(aa_hookhandler,{group_chat_filter,From,To,RPacket,false}),
  handle_cast({group_chat_filter,From,To,RPacket,false},#state{}),
  case ejabberd_router:route(From, To, RPacket) of
    ok ->
      {ok,ok};
    Err ->
      {error,Err}
  end.

%% 20141115 : 这里要优化一下调用缓存模块的地方
%% 将节点信息保存在上下文里，而不是 aa_hookhandler 进程里
%% aa_hookhandler 进程就是瓶颈
ecache_cmd(Cmd) ->
  ?DEBUG("==== function__ecache_cmd ===> ~n\tCmd=~p",[Cmd]),
  Node = case application:get_env(ecache,node) of 
    {ok,N0} ->
      N0;
    _ ->
      [Domain|_] = ?MYHOSTS,
      N = ejabberd_config:get_option({ecache_node,Domain},fun(V)->V end),
      net_adm:ping(N),
      N
  end,
  case catch rpc:call(Node,ecache_main,cmd,[Cmd]) of 
    {'EXIT',Err} ->
      net_adm:ping(Node),
      ?ERROR_MSG("ecache_cmd_exception____~p",[Err]);
    Rtn ->
      Rtn
  end.

mysql_cmd(Sql)->
  ?DEBUG("==== function__ecache_cmd ===> Cmd=~p",[Sql]),
  Node = case application:get_env(ecache,node) of 
    {ok,N0} ->
      N0;
    _ ->
      [Domain|_] = ?MYHOSTS,
      N = ejabberd_config:get_option({ecache_node,Domain},fun(V)->V end),
      net_adm:ping(N),
      N
  end,
  case catch rpc:call(Node,mysql_main,cmd,[Sql],30000) of 
    {'EXIT',Err} ->
      net_adm:ping(Node),
      ?ERROR_MSG("mysql_cmd_exception____~p",[Err]);
    Rtn ->
      Rtn
  end.



%% =================================================
%% 每次消息发送前保存离线消息
%% =================================================
  
save_offline(K,From,#jid{user=TU}=To,Packet) ->
  [JSON] = aa_log:get_text_message_from_packet(Packet),
  StructJson = jiffy:decode(erlang:list_to_binary(JSON)),
  ?WARNING_MSG("----- save From To ~p",[{From, To}]),
  
  %%==================================
  %% 不保存离线消息的消息类型
  %%==================================
  
  case ej:get({<<"type">>},StructJson) of
    <<"130">> ->  
      skip;
    <<"131">> ->
      skip;    
    <<"125">> ->
      skip;
    <<"126">> ->
      skip;
    _->
      % {M,S,SS} = now(), 
      % MsgTime = lists:sublist(erlang:integer_to_list(M*1000000000000+S*1000000+SS),1,13),
      % {Tag,E,Attr,Body} = Packet,
      % -------------------------------------------------
      % 莫名其妙的更换msgtime，这个msgtime对生成处有很高的要求的
      % -------------------------------------------------
      %% lifang log test
      % [OldMsgtime] = [V ||{K1,V}<-Attr,K1== "msgTime"],
      % ?DEBUG("save offline oldmsgtime .. 1  ~p",[OldMsgtime]),
      % ?DEBUG("save offline oldmsgtime .. 2  ~p",[MsgTime]),
      % 
      % RAttr1 = [{K1,V}||{K1,V}<-Attr,K1=/="msgTime"],
      % RAttr2 = [{"msgTime",MsgTime} | RAttr1],
      % RPacket = {Tag,E,RAttr2,Body},
      RPacket = add_msgTime(Packet),
      V = term_to_binary({From,To,RPacket}),
      %  ?INFO_MSG("==== sync_packet ===> insert K=~p~nV=~p",[K,V]),
      %% user＝1并且非群聊时，特殊处理，有效期设为2天
      IS_GROUP_CHAT = aa_group_chat:is_group_chat(To), 
      EX = case is_little_secretary(TU) of 
        true when IS_GROUP_CHAT=:=false ->
          ?LITTLE_SECRETARY_EXPIRE;
        false ->  
          ?NORMAL_EXPIRE 
      end,
      Cmd = ["SETEX",K,integer_to_list(EX),V],
      R = ecache_cmd(Cmd),
      aa_offline_mod:offline_message_hook_handler(save,From,To,RPacket),
      R
  end.

%% 修改packet的content提示内容

%% set_content(Packet) ->
%%   [Json] = aa_log:get_text_message_from_packet(Packet),
%%   {ok,Json1,_} = rfc4627:decode(erlang:list_to_binary(Json)),
%%   ApplicantBin = rfc4627:get_field(Json1,"invitename", <<"skip">>),
%%   Applicant = erlang:binary_to_list(ApplicantBin),
%%   Content = list_to_binary(aa_super_group_chat:get_itdoc_str("zh","system_105001",[Applicant])),
%%   Json2 = rfc4627:set_field(Json1, "type", <<"1000">>),
%%   Json3 = rfc4627:set_field(Json2,"content",Content),
%%   Json4 = rfc4627:encode(Json3),
%%   Json5 = erlang:list_to_binary(Json4),
%%   Body = [{xmlel,<<"body">>,[],[{xmlcdata,Json5}]}],
%%   _Packet2 = Packet#xmlelement{children = Body}.


%% =================================================
%% 处理系统消息不同的类型,并发送
%% =================================================

handle_system_message(#jid{server=Domain,user =FUser }=From,#jid{user=TUser,server=TDomain}=To,Packet) ->
  %% groupchat and msgtype=system body is json format
  [JSON] = aa_log:get_text_message_from_packet(Packet), 
  StructJson = jiffy:decode(erlang:list_to_binary(JSON)),
  case ej:get({<<"type">>},StructJson) of 
    undefined ->
      skip;
    <<"0">> ->
      ToKey = <<"grouplist">>,
      send_to_group(From,Packet,ToKey,StructJson);
    <<"3">> ->
      ToKey = <<"toinvitedlist">>,
      send_to_group(From,Packet,ToKey,StructJson);

    <<"7">> ->
      ToKey = <<"applylist">>,
      send_to_group(From,Packet,ToKey,StructJson);

    <<"8">> ->
      ToKey = <<"applylist">>,
      send_to_group(From,Packet,ToKey,StructJson);
    
    <<"13">> ->
      ToKey = <<"grouplist">>,
      send_to_group(From,Packet,ToKey,StructJson);
   
    <<"14">> ->
       ToKey = <<"grouplist">>,
       send_to_group(From,Packet,ToKey,StructJson);
      
    <<"15">> ->
      ToKey = <<"grouplist">>,
      send_to_group(From,Packet,ToKey,StructJson);
    
    <<"18">> ->
      ToKey = <<"grouplist">>,
      send_to_group(From,Packet,ToKey,StructJson);
    
    <<"19">> ->
      ToKey = <<"grouplist">>,
      send_to_group(From,Packet,ToKey,StructJson);
    
    <<"20">> ->
      %% 140823 : add by liangc 
      %% 宇庭新需求：社交圈新动态消息（ejabber需要处理）      apns推送内容（无） 
      ToKey = <<"grouplist">>,
      %% ToList = rfc4627:get_field(StructJson, ToKey),
      send_to_group(From,Packet,ToKey,StructJson);

    <<"21">> ->
      %% 140925 : add by liangc 
      %% 宇庭新需求：
      %% ejabber服务器需要给applylist列表发送的消息格式（ejabber转发时去掉applylist）
      ToKey = <<"applylist">>,
      %% ToList = rfc4627:get_field(StructJson, ToKey),
      send_to_group(From,Packet,ToKey,StructJson);

    <<"22">> ->
      %% 2015-01-25
      %% 有人被邀请加入多人会话或者有人被踢出的消息（ejabber需要处理）groupmember 
      %% 最多传递5个人的数据（用来显示头像拼接成多人对话图片）      apns推送内容（无）        
      %% content字段： 如果是有人被邀请加入讨论组：xx,yy,zz被邀请加入群组     如果是有人被踢出：xx被管理员移出群组
      ToKey = <<"grouplist">>,
      %% ToList = rfc4627:get_field(StructJson, ToKey),
      send_to_group(From,Packet,ToKey,StructJson);

    <<"23">> ->
      %% 2015-01-25
      %% 好友推荐的消息（ejabber需要处理） apns推送内容（无）  
      ToKey = <<"grouplist">>,
      send_to_group(From,Packet,ToKey,StructJson);
    
    % -------------------------------------------------
    % 以下是新群组的消息处理
    % -------------------------------------------------
    <<"101">> -> %%邀请他人入群
      ToKey = <<"grouplist">>,
      case ej:get({ToKey},StructJson) of
        undefined ->
          skip;
        ToList ->
          JO_1 = ej:delete({ToKey},StructJson),
          lists:foreach(fun(To3) ->
                UID = ej:get({<<"uid">>},To3,<<"skip">>),
                Source = ej:get({<<"source">>},To3, <<"skip">>),
                JID3=#jid{user=UID,server=Source,luser=UID,
                  lserver=Source,resource = <<>>,lresource = <<>>},
                {Rpacket,J4B} = aa_super_group_chat:set_special_group_data(TDomain,JID3, Packet,JO_1),
                route_3(To,JID3,Rpacket,J4B)  
            end,ToList) 
      end;
    <<"102">> -> %%同意入群
          Inviteid = ej:get({<<"inviteid">>},StructJson,<<"skip">>),
          Source = ej:get({<<"invitesource">>},StructJson,<<"skip">>),
          UID = Inviteid, 
          JID3=#jid{user=UID,server=Source,luser=UID,
                  lserver=Source,resource = <<>>,lresource = <<>>},
          {Rpacket,J4B} = aa_super_group_chat:set_special_group_data(TDomain,JID3, Packet,StructJson),
          route_3(To,JID3,Rpacket,J4B), 
          NewJO = aa_super_group_chat:set_super_group_msgsource(TDomain, StructJson),
          NewJson = jiffy:encode(NewJO),
          Body = [{xmlel,<<"body">>,[],[{xmlcdata,NewJson}]}],
          aa_super_group_chat:route_group_msg(From, To, Packet#xmlel{children = Body});
    <<"103">> -> %%拒绝入群
       Inviteid = ej:get({<<"inviteid">>},StructJson,<<"skip">>),
       Source = ej:get({<<"invitesource">>},StructJson,<<"skip">>),
       UID = Inviteid,
       JID3=#jid{user=UID,server=Source,luser=UID,
               lserver=Source,resource = <<>>,lresource = <<>>},
       {Rpacket,J4B} = aa_super_group_chat:set_special_group_data(TDomain, JID3, Packet,StructJson),
       route_3(To,JID3,Rpacket,J4B);
    <<"104">> -> %%申请入群
       ToKey = <<"managerlist">>,
       case ej:get({ToKey},StructJson) of
         undefined ->
           skip;
         ToList ->
           JO_1 = ej:delete({ToKey},StructJson),
           lists:foreach(fun(To3) ->
                 UID = ej:get({<<"uid">>},To3,<<"skip">>),
                 Source = ej:get({<<"source">>},To3, <<"skip">>),
                 JID3=#jid{user=UID,server=Source,luser=UID,
                   lserver=Source,resource = <<>>,lresource = <<>>},
                 {Rpacket,J4B} = aa_super_group_chat:set_special_group_data(TDomain, JID3, Packet,JO_1),
                 route_3(To,JID3,Rpacket,J4B)  
             end,ToList) 
       end;
    <<"105">> ->  %%发给申请人
       Inviteid = ej:get({<<"inviteid">>},StructJson, <<"skip">>),
       NewType = <<"1000">>,
       Countentstr = list_to_binary(aa_super_group_chat:get_itdoc_str("zh","system_105002",[])),
       Jx = ej:set({<<"content">>},StructJson,Countentstr),
       JO_2 = ej:set({<<"type">>},Jx, NewType),
       Uid = Inviteid,
       Source = ej:get({<<"invitesource">>},StructJson,<<"skip">>),
       JID = #jid{user=Uid, server=Source,luser=Uid,
               lserver=Source, resource = <<>>,lresource = <<>>},
       {Rpacket,J4B} = aa_super_group_chat:set_special_group_data(TDomain, JID, Packet,JO_2),
       ?WARNING_MSG("105 105 105 105 ~p",[{To, JID}]), 
       route_3(To,JID,Rpacket,J4B),
       %% Packet2 = set_content(Packet),
       aa_super_group_chat:route_group_msg(From, To, Packet);
    <<"106">> ->  %%发给
       ToKey = <<"inviteid">>,
       case ej:get({ToKey},StructJson) of
         undefined ->
           skip;
         Inviteid ->
           Source = ej:get({<<"invitesource">>},StructJson,<<"skip">>),
           case is_integer(Inviteid) of
             true ->
               Uid = integer_to_list(Inviteid);
             _ ->
               Uid = binary_to_list(Inviteid)
           end,
           JID = #jid{user=Uid,server=Source,luser=Uid,
                   lserver=Source,resource = <<>>,lresource = <<>>},
           {Rpacket,J4B} = aa_super_group_chat:set_special_group_data(TDomain, JID, Packet,StructJson),
           route_3(To,JID,Rpacket,J4B)
       end;
    <<"107">> ->
       ToKey = <<"managerlist">>,
       case ej:get({ToKey},StructJson) of
         undefined ->
           skip;
         ToList ->
           #jid{luser=Luser} = From,
           JO_1 = ej:delete({ToKey},StructJson),
           Username = ej:get({<<"username">>},StructJson),
           Args = [binary_to_list(jiffy_utf8:fix(Username))],
           NewType = <<"1000">>,
           Countentstr = list_to_binary(aa_super_group_chat:get_itdoc_str("zh","system_107",Args)),
           Managerlist = lists:delete(list_to_binary(Luser), ToList),
           Jx = ej:set({<<"content">>},JO_1,Countentstr),
           JO_2 = ej:set({<<"type">>},Jx, NewType),
           lists:foreach(fun(To3) ->
                 UID = ej:get({<<"uid">>},To3,<<"skip">>),
                 Source = ej:get({<<"source">>},To3, <<"skip">>),
                 JID3=#jid{user=UID,server=Source,luser=UID,
                   lserver=Source,resource = <<>>,lresource = <<>>},
                 {Rpacket,J4B} = aa_super_group_chat:set_special_group_data(TDomain, JID3, Packet,JO_2),
                 route_3(To,JID3,Rpacket,J4B)  
           end,Managerlist) 
       end;
    <<"108">> -> %%踢人
       ToKey = "kickid",
       case ej:get({ToKey},StructJson) of
         undefined ->
           skip;
         Inviteid ->
           Source = ej:get({<<"kicksource">>},StructJson, <<"skip">>),
           J4B = erlang:list_to_binary(JSON),
           UID = binary_to_list(Inviteid),
           JID = #jid{user=UID,server=Source,luser=UID,
                   lserver=Source,resource = <<>>,lresource = <<>>},
           route_3(To,JID,Packet,J4B),
           aa_super_group_chat:route_group_msg(From, To, Packet)
       end;
    <<"109">> -> %%群组解散
       ToKey = <<"grouplist">>,
       case ej:get({ToKey},StructJson) of
         undefined ->
           skip;
         ToList ->
           JO_1 = ej:delete({ToKey},StructJson),
           RMasklist = ej:get({<<"masklist">>},JO_1),
           lists:foreach(fun(To3) ->
                 UID = ej:get({<<"uid">>},To3,<<"skip">>),
                 Source = ej:get({<<"source">>},To3, <<"skip">>),
                 if
                   UID =/= FUser->
                     case RMasklist of
                       undefined ->
                         J4B = jiffy:encode(JO_1);
                       Masklist ->
                         case lists:member(To3, Masklist) of
                           true->
                             JO_2 = ej:set({<<"mask">>},JO_1, <<"1">>);
                           _->
                             JO_2 = ej:set({<<"mask">>},JO_1, <<"0">>)
                         end,
                         J4B = jiffy:encode(JO_2)
                     end,
                     
                     JID3=#jid{user=UID,server=Source,luser=UID,
                           lserver=Source,resource = <<>>,lresource = <<>>},
                     route_3(To,JID3,Packet,J4B);
                   true->
                     skip
                 end
           end,ToList) 
         end;
      <<"122">> -> %%新群组内红包领取
        ToKey = <<"sendid">>,
        case ej:get({ToKey},StructJson) of
          undefined ->
            skip;
          Sendid ->
            JO_1 = ej:set({<<"type">>},StructJson,<<"1000">>),
            {Xp,Ep,Ap,_} = Packet,
            J4B = jiffy:encode(JO_1),
            Body = [{xmlel,<<"body">>,[],[{xmlcdata,J4B}]}],
            RPacket = {Xp,Ep,Ap,Body},
            JID3=#jid{user=Sendid,server=Domain,luser=Sendid,
                   lserver=Domain,resource = <<>>,lresource = <<>>},
            route_3(To,JID3,RPacket,J4B)
      end;
      
    <<"125">> -> %%亿企播关闭专用
       ToKey = <<"grouplist">>,
       Grouplist = ej:get({ToKey},StructJson, []),
       JO_1 = ej:delete({ToKey},StructJson),
       J4B = jiffy:encode(JO_1),
       lists:foreach(fun(UID)->
                     JID3=#jid{user=UID,server=Domain,luser=UID,
                           lserver=Domain,resource = <<>>,lresource = <<>>},
                     route_3(To,JID3,Packet,J4B)
         end, Grouplist);
        
    <<"126">> -> %% 亦语接口：添加直播开启的消息指定grouplist消息通知
       ToKey = <<"grouplist">>,
       case ej:get({ToKey},StructJson) of
         undefined ->
           skip;
         ToList ->
           JO_1 = ej:delete({ToKey},StructJson),
           J4B = jiffy:encode(JO_1),
           lists:foreach(fun(UID) ->
                 JID3=#jid{user=UID,server=Domain,luser=UID,
                   lserver=Domain,resource = <<>>,lresource = <<>>},
                 route_3(To,JID3,Packet,J4B) 
             end,ToList) 
       end;
    % -------------------------------------------------
    % 以上是新群组的消息处理
    % -------------------------------------------------
    <<"130">> ->
       %% 2016-01-17
       %% 好友推荐的消息（ejabber需要处理） apns推送内容（无）  
       ToKey = <<"grouplist">>,
       send_to_group(From,Packet,ToKey,StructJson);
      
    % -------------------------------------------------
    % 这是什么功能，要发送所有在线的人
    % -------------------------------------------------
    <<"131">> ->
       {TL3} = StructJson, 
       % TL3_1=lists:map(fun({K3,V3})-> case K3=:=ToKey of true->skip;false->{K3,V3} end end,TL3),
       TL3_2=[X||X<-TL3,X=/=skip],
       JO_1 = {TL3_2},
       J4B = jiffy:encode(JO_1),
       ToList = ejabberd_sm:get_vh_session_list(Domain),
       lists:foreach(fun({U, S, R}) ->
             if 
               S =:= Domain ->
                 JID3=#jid{user=U,server=Domain,luser=U,
                   lserver=Domain,resource=R,lresource=R},
                 route_3(To,JID3,Packet,J4B);
               true->
                 skip
             end
         end,ToList); 
    <<"132">> -> %%新群组内文件相关的消息
       JO_1 = ej:set({<<"type">>},StructJson,<<"1000">>),
       {Xp,Ep,Ap,_} = Packet,
       J4B = jiffy:encode(JO_1),
       Body = [{xmlel,<<"body">>,[],[{xmlcdata,J4B}]}],
       RPacket = {Xp,Ep,Ap,Body},
       aa_super_group_chat:route_group_msg(From, To, RPacket);
    
    %%=================================
    %% 通用类型处理
    %%=================================
    _ ->
      case TUser =:= <<"0">> of
        true ->
          skip;
        false ->
          case aa_group_chat:check_group_type(TDomain) of
            <<"group">> ->
              aa_group_chat:route_group_msg(From,To,Packet);
            <<"super_group">> ->
              aa_super_group_chat:route_group_msg(From, To, Packet);
            _->
              skip
          end
      end
  end.

send_to_group(#jid{server=Domain,user =_FUser }=From,Packet,ToKey,StructJson) ->
  case ej:get({ToKey},StructJson) of
    undefined ->
      skip;
    ToList ->
      JO_1 = ej:delete({ToKey}, StructJson),
      Json = jiffy:encode(JO_1),
      lists:foreach(fun(UID) ->
            JID3=#jid{user=UID,server=Domain,luser=UID,
              lserver=Domain,resource = <<>>,lresource = <<>>},
            route_3(From,JID3,Packet,Json) 
        end,ToList) 
  end.

%% =================================================
%% 特殊type，不显示内容的，添加隐藏标志
%% 并保存离线消息，对特殊类型不用保存
%% =================================================

sync_hide(#jid{user=FU,server=FD,resource=_FR}=From,#jid{user=TU,server=TD}=To,Packet,[Mid]) ->
    Fromstr = <<FU/binary,"@",FD/binary>>,
    Tostr = <<TU/binary,"@",TD/binary>>,
    [JSON] = aa_log:get_text_message_from_packet(Packet),
    StructJson = jiffy:decode(erlang:list_to_binary(JSON)), 
    <<IsHide:9/binary,_RestId/binary>> = Mid,
    case IsHide of
      <<"hide_msg_">> ->
        SYNCID = <<Mid/binary,"@",TD/binary>>,    %不是ack@后为TD
        Rpacket = Packet;
      _->
        case ej:get({<<"type">>},StructJson) of
          <<"19">> ->
            SRC_ID_STR  = <<"hide_msg_",Mid/binary>>,
            SYNCID = <<SRC_ID_STR/binary,"@",TD/binary>>,
            Rpacket = fxml:replace_tag_attr(<<"id">>, SRC_ID_STR, Packet);
          <<"20">> ->
            SRC_ID_STR  = <<"hide_msg_",Mid/binary>>,
            SYNCID = <<SRC_ID_STR/binary,"@",TD/binary>>,
            Rpacket = fxml:replace_tag_attr(<<"id">>, SRC_ID_STR, Packet);
          _->
            case aa_packet_filter:get_cache_mask(Fromstr, Tostr) of
              ["1",_,_,_]->
                SRC_ID_STR  = <<"hide_msg_", Mid/binary>>,
                SYNCID = <<SRC_ID_STR,"@",TD/binary>>,
                Rpacket = fxml:replace_tag_attr(<<"id">>, SRC_ID_STR, Packet);
              _->
                SYNCID = <<Mid/binary,"@",TD/binary>>,
                Rpacket = Packet
            end
        end
    end,
    save_offline(SYNCID,From,To,Rpacket).


%% =================================================
%% 处理消息回执后的事务操作
%% =================================================

handle_receipt_message(#jid{user=FU,server=FD,resource=FR}=_From,[Mid,State]) ->
  SYNCID = <<Mid/binary,"@",FD/binary>>,   %%ACK 后面跟的是FromDomain
  case is_little_secretary(FU) of 
   false ->  
     case lists:member(FU,[<<"1680028">>,<<"1697379">>,<<"15538">>,<<"15475">>]) of
       false ->  
         KK = << FU/binary,"@",FD/binary, "/offline_msg">>, 
         case ecache_cmd(["GET",<<Mid/binary,"/zip_offline_msg_body">>]) of
           {ok,Obj} when erlang:is_binary(Obj) ->
             Ziplist = erlang:binary_to_term(Obj),
             lists:foreach(fun(Zipid)->
                   ack_sync(Zipid,State,0),
                   ecache_cmd(["ZREM",KK,Zipid])
               end,Ziplist);
           _->
             ack_sync(SYNCID,State,0),
             ecache_cmd(["ZREM",KK,SYNCID]), 
             ?INFO_MSG("[v.141223] ==> SYNC_RES ack => ACK_USER=~p ; ACK_ID=~p",[KK,SYNCID])
         end; 
       _->
         nothing
     end;
   true ->
     ?WARNING_MSG("--6 ",[]),
     %% 不能删除，而是增加一个 hset 
     HK = << FU/binary,"@",FD/binary, FR/binary >>, 
     {M2,S2,SS2} = now(), 
     HV = lists:sublist(erlang:integer_to_list(M2*1000000000000+S2*1000000+SS2),1,13),
     ecache_cmd(["HSET",HK,SYNCID,HV]),
     ?WARNING_MSG("[v.150103] ==> SYNC_RES ack_user_1 => ACK_ID=~p ; V=~p",[SYNCID,HV])
  end.

is_little_secretary(<<"stranger_limit">>) ->
  false;
is_little_secretary(Uid) when is_binary(Uid) ->
  is_little_secretary(binary_to_integer(Uid));
is_little_secretary(Uid) when is_list(Uid) ->
  is_little_secretary(list_to_integer(Uid));
is_little_secretary(Uid) when is_integer(Uid) ->
  if Uid == 1 -> true;
     (Uid >= 1000) and (Uid =< 10000) -> true;
     true -> false
  end.

add_msgTime(Packet) ->
  {M,S,SS} = now(), 
  MsgTime = lists:sublist(erlang:integer_to_list(M*1000000000000+S*1000000+SS),1,13),
  {Tag,E,Attr,Body} = Packet,
  ?DEBUG("replace old msgtime .. ~p",[ej:get({<<"msgTime">>},Attr)]),
  {RAttr1} = ej:set({<<"msgTime">>},{Attr},list_to_binary(MsgTime)),
  {Tag,E,RAttr1,Body}.
%% @author baol
%% @doc @todo Add description to sysn_send.


-module(sysn_send).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-compile(export_all).
-include("include/ejabberd.hrl").
-include("include/jlib.hrl").
-include("include/logger.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
     start_link/0,
     start/0,
     http_send/1,
     sysn_http_send/1,
     offline_msg_send/2]).

start_link()->
  gen_server:start_link(?MODULE, [], []).

start() ->
  sysn_send_sup:start_child().

%%http消息异步
http_send(Data)->
  {ok,Pid} = start(),
  gen_server:cast(Pid, {http_send,Data}).

%%离线消息异步发送 
offline_msg_send(Jid,R)->
  {ok,Pid} = start(),
  gen_server:cast(Pid, {offline_msg_send,Jid,R}).

%%均衡节点异步发送
sysn_http_send(Data)->
  {ok,Pid} = start(),
  gen_server:cast(Pid, {sysn_http_send,Data}).
%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
  Result :: {ok, State}
      | {ok, State, Timeout}
      | {ok, State, hibernate}
      | {stop, Reason :: term()}
      | ignore,
  State :: term(),
  Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
    {ok, #state{}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
  Result :: {reply, Reply, NewState}
      | {reply, Reply, NewState, Timeout}
      | {reply, Reply, NewState, hibernate}
      | {noreply, NewState}
      | {noreply, NewState, Timeout}
      | {noreply, NewState, hibernate}
      | {stop, Reason, Reply, NewState}
      | {stop, Reason, NewState},
  Reply :: term(),
  NewState :: term(),
  Timeout :: non_neg_integer() | infinity,
  Reason :: term().
%% ====================================================================
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
  Result :: {noreply, NewState}
      | {noreply, NewState, Timeout}
      | {noreply, NewState, hibernate}
      | {stop, Reason :: term(), NewState},
  NewState :: term(),
  Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast({http_send,Data},State)->
  {From,To,Stanza} = Data,
  aa_hookhandler:user_send_packet_handler(From,To,Stanza), 
  case ejabberd_router:route(From, To, Stanza) of 
    ok -> 
      Size = erlang:size(erlang:term_to_binary(Stanza)),
      if
        Size =< 1000->
          ?WARNING_MSG("httpmsg ok:~p",[Stanza]);
        true->
          skip
      end;
    _->
      ?ERROR_MSG("httpmsg error:~p",[Stanza])
  end,
  {stop,normal,State};

handle_cast({offline_msg_send,Jid,R},State)->
  T1 = now(),
  deel_offline_msg_send(Jid,R,100,{[],[],[],[],[]}),
  Diff = timer:now_diff(now(), T1),
  ?WARNING_MSG("offline msg~p",[R]),
  ?WARNING_MSG("time diff~p",[Diff]),
  {stop,normal,State};

handle_cast({sysn_http_send,Data},State)->
  sysn_http:deel_http_data(Data),
  {stop,normal,State};

handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
  Result :: {noreply, NewState}
      | {noreply, NewState, Timeout}
      | {noreply, NewState, hibernate}
      | {stop, Reason :: term(), NewState},
  NewState :: term(),
  Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
  Reason :: normal
      | shutdown
      | {shutdown, term()}
      | term().
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
  Result :: {ok, NewState :: term()} | {error, Reason :: term()},
  OldVsn :: Vsn | {down, Vsn},
  Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
deel_offline_msg_send(_,[],_,OfflineMsg)->
  {Normalchat,GroupChat,SuperGroupChat,System,Idlist} = OfflineMsg,
  lists:foreach(fun({Key,Ziplist})->
               Cmd = ["SETEX",Key,integer_to_list(30),erlang:term_to_binary(Ziplist)],
                aa_hookhandler:ecache_cmd(Cmd)
              end, Idlist),
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
  offline_route(Normalchat),
  offline_route(GroupChat),
  offline_route(SuperGroupChat),
  offline_route(System),
  deel_offline_msg_send(Jid,R,200,{[],[],[],[],[]});
deel_offline_msg_send(#jid{lresource = Resource}  = Jid,R,N,OfflineMsg)->
  [Id|LR] = R,
  ?WARNING_MSG("deel_offline_msg_send .. 1 ~n\t R = ~p",[R]),
  {NewOfflineMsg,NewNum} =
    try
      {NormalChatoffline,Groupchat,SuperGroupChat,Systemchatoffline,Idlist} = OfflineMsg,
      case aa_offline_mod:get_offline_msg(Jid, Id) of
        {From,To,Packet}->
          ?WARNING_MSG("deel_offline_msg_send .. 2 ~n\t From = ~p, ~n\t To = ~p, ~n\t Packet = ~p",[From, To, Packet]),
          case check_version(Resource) of      %%新老版本兼容
            new->
              case fxml:get_tag_attr_s(<<"msgtype">>, Packet) of 
                <<"normalchat">> ->
                  {New_NormalChatoffline,ZipIdList} = aa_offline_mod:make_offline(normalchat,{NormalChatoffline,From,To,Packet},{Idlist,Id}),
                  {{New_NormalChatoffline,Groupchat,SuperGroupChat,Systemchatoffline,ZipIdList},N-1};
                <<"groupchat">> ->
                  {New_GroupChatoffline,ZipIdList} = aa_offline_mod:make_offline(groupchat,{Groupchat,From,To,Packet},{Idlist,Id}),
                  {{NormalChatoffline,New_GroupChatoffline,SuperGroupChat,Systemchatoffline,ZipIdList},N-1};
                <<"super_groupchat">> ->
                  {New_SuperGroup,ZipIdList} = aa_offline_mod:make_offline(super_groupchat,{SuperGroupChat,From,To,Packet},{Idlist,Id}),
                  {{NormalChatoffline,Groupchat,New_SuperGroup,Systemchatoffline,ZipIdList},N-1};
                _->
                  {{NormalChatoffline,Groupchat,SuperGroupChat,[{From,To,Packet}|Systemchatoffline],Idlist},0}
              end;
            old->
              {{NormalChatoffline,Groupchat,SuperGroupChat,[{From,To,Packet}|Systemchatoffline],Idlist},N-1}
          end;
        _R->
          ?ERROR_MSG("deel_offline_msg_send .. 3 ~n\t error = ~p",[_R]),
          {OfflineMsg,N-1}
      end
    catch
      E:R->
        ?ERROR_MSG("deel_offline_msg_send .. 4 ~n\t error = ~p,~n\t Reason = ~p, ~n\t stacktrace = ~p",[E,R,erlang:get_stacktrace()]),
        {OfflineMsg,N-1}
    end,
  deel_offline_msg_send(Jid,LR,NewNum,NewOfflineMsg).

offline_route(PacketList)->
  lists:foldr(fun({From,To,Packet},Acc)->
              #jid{lresource = Resource} = To,
              case check_version(Resource) of
                old->
                  ejabberd_router:route(From,To,Packet);
                new->
                  timer:sleep(500),
                  ejabberd_router:route(From,To,Packet)
              end,
              Acc
            end, ok, PacketList).
%%检查离线消息的新老版本
check_version(_Resource)->
  % case Resource of 
  %   ""->
  %     old;
  %   undefined ->
  %     new;
  %   _->
  %     case string:sub_string(Resource, 1, 6) of
  %       "app_v2"->
  %         R = string:sub_string(Resource, 1, 10),
  %         if
  %           R >= "app_v2.1.2"->
  %             new;
  %           true->
  %             old
  %         end;
  %       _->
  %         %% lifang update
  %         %% old
  %         new
  %     end
  % end.
  new.
      


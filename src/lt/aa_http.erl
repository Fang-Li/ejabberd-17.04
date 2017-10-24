-module(aa_http).

-behaviour(gen_server).

-include("include/ejabberd.hrl").
-include("include/jlib.hrl").
-include("include/logger.hrl").

%% API
-export([start_link/0]).

-define(Port,5380).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).
-record(success,{sn,success=true,entity}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

handle_http(Req) ->
	gen_server:call(?MODULE,{handle_http,Req}).

http_response({S,Req}) ->
	try
		Res = {[{sn,S#success.sn},{success,S#success.success},{entity,S#success.entity}]},
		?DEBUG("Res_obj=~p",[Res]),
		J = binary_to_list(jiffy:encode(Res)),
		?DEBUG("Res_json=~p",[J]),
		Req:ok([{"Content-Type", "text/json"}], "~s", [J]) 
	catch
		_:_->
			Err = erlang:get_stacktrace(),
			Req:ok([{"Content-Type", "text/json"}], "~s", ["{\"success\":false,\"entity\":\""++Err++"\"}"]) 
	end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
  Port = ejabberd_config:get_option({sycn_port,?MYNAME},fun(V)->V end),
  ?WARNING_MSG("--------------------------------------------------~p",[Port]),
	misultin:start_link([{port, Port}, {loop, fun(Req) -> handle_http(Req) end}]),
	{ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
%% http://localhost:5380/?body={"method":"process_counter"}
handle_call({handle_http,Req}, _From, State) ->
	Reply = 
		try
		Method = Req:get(method),
		Args = case Method of
			'GET' ->
				Req:parse_qs();
			'POST' ->
				Req:parse_post()
		end,
		?DEBUG("http_ARGS ::> ~n~p",[Args]),	 
		Body = case Args of 
			       [{"body",P0}]->
				       	P0;
			       [{"body",[],P1}] ->
					P1;
			       O->
					?INFO_MSG("error_args ::>~n~p",[Args]), 
          throw("body error")
		end,
%% 		[{"body",Body}] = Args,
		StructJson = jiffy:decode(Body),
		?INFO_MSG("http ::> body=~p",[Body]),	 
		M = ej:get({<<"method">>},StructJson),
		SN = case ej:get({<<"sn">>},StructJson) of 
			undefined ->	
				{M1,S1,SS1} = now(),
			    integer_to_list(M1*1000000000000+S1*1000000+SS1);
			SN_115 ->
				binary_to_list(SN_115)
		end,
		S = case ej:get({<<"service">>},StructJson) of undefined -> none ; SS -> binary_to_list(SS)  end,
		case binary_to_list(M) of 
			"process_counter" ->
				Counter = aa_process_counter:process_counter(),
				http_response({#success{sn=list_to_binary(SN),success=true,entity=Counter},Req});
			"msg_counter" when S =:= "withdate"->
				try
					P  = ej:get({<<"params">>},StructJson),
					Yo = ej:get({<<"y">>},P),
					Mo = ej:get({<<"m">>},P),
					Do = ej:get({<<"d">>},P),
					CounterList = aa_process_counter:msg_counter(Yo,Mo,Do),
					http_response({#success{sn=list_to_binary(SN),success=true,entity=CounterList},Req})
				catch
					_:_->
						Err = erlang:get_stacktrace(),
						?WARNING_MSG("msg_counter:~p",[Err]),
						http_response({#success{sn=list_to_binary(SN),success=false,entity=list_to_binary("error")},Req})
				end;
			"add" when S =:= "blacklist" ->
				?INFO_MSG("http blacklist.add ::> ~p",[Args]),
				try
					P = ej:get({<<"params">>},StructJson),
					From = ej:get({<<"from">>},P),
					To   = ej:get({<<"to">>},P),
					
					aa_blacklist:add(From,To),
					http_response({#success{sn=list_to_binary(SN),success=true,entity=list_to_binary("ok")},Req}) 
				catch 
					_:_->
						Err = erlang:get_stacktrace(),
						http_response({#success{sn=list_to_binary(SN),success=false,entity=list_to_binary(Err)},Req}) 
				end;
			"reload" when S =:= "blacklist" ->
				?INFO_MSG("http blacklist.remove ::> ~p",[Args]),
				try
					P = ej:get({<<"params">>},StructJson),
					From = ej:get({<<"from">>},P),
					To   = ej:get({<<"to">>},P),
					aa_blacklist2:remove(From,To),
					http_response({#success{sn=list_to_binary(SN),success=true,entity=list_to_binary("ok")},Req}) 
				catch 
					_:_->
						Err = erlang:get_stacktrace(),
						http_response({#success{sn=list_to_binary(SN),success=false,entity=list_to_binary(Err)},Req}) 
				end;
			"get" when S =:= "blacklist" ->
				?INFO_MSG("http blacklist.get ::> ~p",[Args]),
				try
					P = ej:get({<<"params">>},StructJson),
					JID = ej:get({<<"jid">>},P),
					BList = aa_blacklist:get_list(JID),
					http_response({#success{sn=list_to_binary(SN),success=true,entity=BList},Req}) 
				catch 
					_:_->
						Err = erlang:get_stacktrace(),
						http_response({#success{sn=list_to_binary(SN),success=false,entity=list_to_binary(Err)},Req}) 
				end;
			"with" when S =:= "blacklist" ->
				?INFO_MSG("http blacklist.with ::> ~p",[Args]),
				try
					P = ej:get({<<"params">>},StructJson),
					JID = ej:get({<<"jid">>},P),
					BList = aa_blacklist:get_with(JID),
					http_response({#success{sn=list_to_binary(SN),success=true,entity=BList},Req}) 
				catch 
					_:_->
						Err = erlang:get_stacktrace(),
						http_response({#success{sn=list_to_binary(SN),success=false,entity=list_to_binary(Err)},Req}) 
				end;
			"reload" when S =:= "super_group_user"->
				?INFO_MSG("http super_group_user.reload ::> ~p",[Args]),
				try
					P = ej:get({<<"params">>},StructJson),
					GID = ej:get({<<"gid">>},P),
					Domain = ej:get({<<"domain">>},P),
					
					case aa_super_group_chat:reload_group_user(Domain,GID) of 
						{_,_,_,_,_,_,_} ->
							http_response({#success{sn=list_to_binary(SN),success=true,entity=list_to_binary("ok")},Req});
						_ ->
							http_response({#success{sn=list_to_binary(SN),success=false,entity=list_to_binary("callback_error")},Req}) 
					end
				catch
					_:_->
						Err = erlang:get_stacktrace(),
						?ERROR_MSG("group_user.reload.error ~p",[Err]),
						http_response({#success{sn=list_to_binary(SN),success=false,entity=exception},Req})
				end;
			"reload" when S =:= "group_user" ->
				?INFO_MSG("http group_user.reload ::> ~p",[Args]),
				try
					P = ej:get({<<"params">>},StructJson),
					GID = ej:get({<<"gid">>},P),
					Domain = ej:get({<<"domain">>},P),
					case aa_group_chat:reload_group_user(Domain,GID) of 
						{_,_,_,_,_} ->
							http_response({#success{sn=list_to_binary(SN),success=true,entity=list_to_binary("ok")},Req});
						_ ->
							http_response({#success{sn=list_to_binary(SN),success=false,entity=list_to_binary("callback_error")},Req}) 
					end
				catch 
					_:_->
						Err = erlang:get_stacktrace(),
						?ERROR_MSG("group_user.reload.error ~p",[Err]),
						http_response({#success{sn=list_to_binary(SN),success=false,entity=exception},Req}) 
				end;
			"reload" when S =:= "mask" ->
				?INFO_MSG("http mask.reload ::> ~p",[Args]),
				try
					P = ej:get({<<"params">>},StructJson),
					MASK_FROM = ej:get({<<"from">>},P),
					MASK_TO = ej:get({<<"to">>},P),
					MASK_FROM_STR = case is_binary(MASK_FROM) of true -> binary_to_list(MASK_FROM); _-> MASK_FROM end,
					MASK_TO_STR = case is_binary(MASK_TO) of true -> binary_to_list(MASK_TO); _-> MASK_TO end,
					case aa_packet_filter:reload(mask,MASK_FROM_STR,MASK_TO_STR) of 
						ok ->
							http_response({#success{sn=list_to_binary(SN),success=true,entity=list_to_binary("ok")},Req});
						_ ->
							http_response({#success{sn=list_to_binary(SN),success=false,entity=list_to_binary("callback_error")},Req}) 
					end
				catch 
					_:_->
						Err = erlang:get_stacktrace(),
						?ERROR_MSG("reload__mask.error sn=~p ; exception=~p",[SN,Err]),
						http_response({#success{sn=list_to_binary(SN),success=false,entity=exception},Req}) 
				end;
			"reload_all" when S =:= "mask" ->
				?INFO_MSG("http mask.reload_all ::> ~p",[Args]),
				try
					P = ej:get({<<"params">>},StructJson),
					MASK_TO = ej:get({<<"to">>},P),
					MASK_TO_STR = case is_binary(MASK_TO) of true -> binary_to_list(MASK_TO); _-> MASK_TO end,
					case aa_packet_filter:reload_all(mask,MASK_TO_STR) of 
						ok ->
							http_response({#success{sn=list_to_binary(SN),success=true,entity=list_to_binary("ok")},Req});
						_ ->
							http_response({#success{sn=list_to_binary(SN),success=false,entity=list_to_binary("callback_error")},Req}) 
					end
				catch 
					_:_->
						Err = erlang:get_stacktrace(),
						?ERROR_MSG("reload__mask.error sn=~p ; exception=~p",[SN,Err]),
						http_response({#success{sn=list_to_binary(SN),success=false,entity=exception},Req}) 
				end;
			"reload" when S =:= "friend_log" ->
				?INFO_MSG("http friend_log.reload ::> ~p",[Args]),
				try
					P = ej:get({<<"params">>},StructJson),
					MASK_FROM = ej:get({<<"from">>},P),
					MASK_TO = ej:get({<<"to">>},P),
					MASK_FROM_STR = case is_binary(MASK_FROM) of true -> binary_to_list(MASK_FROM); _-> MASK_FROM end,
					MASK_TO_STR = case is_binary(MASK_TO) of true -> binary_to_list(MASK_TO); _-> MASK_TO end,
					case aa_packet_filter:reload(friend_log,MASK_FROM_STR,MASK_TO_STR) of 
						ok ->
							http_response({#success{sn=list_to_binary(SN),success=true,entity=list_to_binary("ok")},Req});
						_ ->
							http_response({#success{sn=list_to_binary(SN),success=false,entity=list_to_binary("callback_error")},Req}) 
					end
				catch 
					_:_->
						Err = erlang:get_stacktrace(),
						?ERROR_MSG("reload__mask.error sn=~p ; exception=~p",[SN,Err]),
						http_response({#success{sn=list_to_binary(SN),success=false,entity=exception},Req}) 
				end;
			"reload" when S =:= "opt_userlist" ->
				try
					Return = aa_mongodb:set_opt_userlist(),
					http_response({#success{sn=list_to_binary(SN),success=true,entity=erlang:term_to_binary(Return)},Req}) 
				catch 
					_:_->
						Err = erlang:get_stacktrace(),
						http_response({#success{sn=list_to_binary(SN),success=false,entity=list_to_binary(Err)},Req}) 
				end;
			"add" when S =:= "be_blocked"->
				try
					P = ej:get({<<"params">>},StructJson),
					BLOCKEDID_LIST = ej:get({<<"blockedid_list">>},P),
					TIME_LIST = ej:get({<<"time_list">>},P),
					TypeList = ej:get({<<"type_list">>},P),
					case aa_shielding_system:add_blocked(BLOCKEDID_LIST,TIME_LIST,TypeList) of 
						ok ->
							http_response({#success{sn=list_to_binary(SN),success=true,entity=list_to_binary("ok")},Req});
						_ ->
							http_response({#success{sn=list_to_binary(SN),success=false,entity=list_to_binary("callback_error")},Req}) 
					end
				catch 
					_:_->
						Err = erlang:get_stacktrace(),
						?ERROR_MSG("add.be_blocked.error sn=~p ; exception=~p",[SN,Err]),
						http_response({#success{sn=list_to_binary(SN),success=false,entity=exception},Req}) 
				end;
			"remove" when S =:= "be_blocked"->
				try
					P = ej:get({<<"params">>},StructJson),
					BLOCKEDID_LIST = ej:get({<<"blockedid_list">>},P),
					TypeList = ej:get({<<"type_list">>},P),
					case aa_shielding_system:remove_blocked(BLOCKEDID_LIST,TypeList) of
							ok ->
								http_response({#success{sn=list_to_binary(SN),success=true,entity=list_to_binary("ok")},Req});
							_ ->
								http_response({#success{sn=list_to_binary(SN),success=false,entity=list_to_binary("callback_error")},Req}) 
					end
				catch 
					_:_->
						Err = erlang:get_stacktrace(),
						?ERROR_MSG("remove.be_blocked.error sn=~p ; exception=~p",[SN,Err]),
						http_response({#success{sn=list_to_binary(SN),success=false,entity=exception},Req}) 
				end;
			"add" when S =:= "stranger_limit"->
        try
          P = ej:get({<<"params">>},StructJson),
		  Uid = ej:get({<<"uid">>},P),
		  Domain = ej:get({<<"domain">>},P),
		  Limit = ej:get({<<"limit">>},P),
          case aa_stranger_limit:set_stranger_limit(Uid,Limit,Domain) of
            ok->
              http_response({#success{sn=list_to_binary(SN),success=true,entity=list_to_binary("ok")},Req});
            _->
              http_response({#success{sn=list_to_binary(SN),success=false,entity=list_to_binary("callback_error")},Req})
          end
        catch
          _:_->
            Err = erlang:get_stacktrace(),
            ?ERROR_MSG("add.stranger_limit.error sn=~p ; exception=~p",[SN,Err]),
            http_response({#success{sn=list_to_binary(SN),success=false,entity=exception},Req})
        end;
      "config" when S =:= "push" ->
        try
          P = ej:get({<<"params">>},StructJson),
          case aa_config:push_config(P) of
            ok ->
              http_response({#success{sn=list_to_binary(SN),success=true,entity=list_to_binary("ok")},Req});
            _->
              http_response({#success{sn=list_to_binary(SN),success=false,entity=list_to_binary("config push error")},Req})
          end
        catch
          _:_->
            Err = erlang:get_stacktrace(),
            ?ERROR_MSG("add.stranger_limit.error sn=~p ; exception=~p",[SN,Err]),
            http_response({#success{sn=list_to_binary(SN),success=false,entity=exception},Req})
        end;
      _ ->
				http_response({#success{sn=list_to_binary(SN),success=false,entity=list_to_binary("method undifine")},Req})
		end
	catch
    throw:_->
      skip;
		_:Reason -> 
			?INFO_MSG("==== aa_http_normal ====~p",[{Reason,erlang:get_stacktrace()}]) 
	end,
	{reply,Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


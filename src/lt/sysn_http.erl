%% @author baol
%% @doc @todo Add description to sysn_http.


-module(sysn_http).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("include/ejabberd.hrl").
-include("include/logger.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).

-export([do_http/1,deel_http_data/1]).

start_link()->
	gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

do_http(Data)->
	gen_server:cast(?MODULE,{sysn_http_send,Data}),
	ok.

deel_http_data(Data)->
	Ds = lists:sort(Data),
	lists:foreach(fun(D)->
						  	try
							  case D of
							  	{_,xmpp,Xmpp}->
										spawn(fun(Xmpp)-> deel_xmpp(Xmpp) end);
								  {_,api,Api}->
										spawn(fun(Xmpp)->deel_api(Api) end);
								  _->
									  []
							  end
							catch
								E:R->
									?ERROR_MSG("deel_http_data error ~p",[{E,R}])
							end
						  end, Ds).

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
handle_cast({sysn_http_send,Data}, State) ->
	?WARNING_MSG("Data~p",[Data]),
	sysn_send:sysn_http_send(Data),
    {noreply, State};

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
code_change(_OldVsn, State,_Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

deel_xmpp(Xmpp)->
	Stanza = xml_stream:parse_element(Xmpp),
	From = jlib:string_to_jid(fxml:get_tag_attr_s(<<"from">>, Stanza)),
	To = jlib:string_to_jid(fxml:get_tag_attr_s(<<"to">>, Stanza)),
	{xmlel,<<"message">>, _Attrs, _Kids} = Stanza,
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
	end.

deel_api(Args)->
	Body = case Args of 
					       [{"body",P0}]->
						       	P0;
					       [{"body",[],P1}] ->
							P1;
					       _->
							""
				end,
	{ok,Obj,_Re} = rfc4627:decode(Body),
	{ok,M} = rfc4627:get_field(Obj, "method"),
	S = case rfc4627:get_field(Obj, "service") of {ok,SS} -> binary_to_list(SS); _-> none end,
	case binary_to_list(M) of 
	  "add" when S =:= "blacklist" ->
				{ok,P} = rfc4627:get_field(Obj, "params"),
				{ok,From} = rfc4627:get_field(P, "from"),
				{ok,To} = rfc4627:get_field(P, "to"),
				aa_blacklist:add(From,To);
	  "remove" when S =:= "blacklist" ->
				{ok,P} = rfc4627:get_field(Obj, "params"),
				{ok,From} = rfc4627:get_field(P, "from"),
				{ok,To} = rfc4627:get_field(P, "to"),
				aa_blacklist:remove(From,To);
	  "reload" when S =:= "super_group_user"->
				{ok,P} = rfc4627:get_field(Obj, "params"),
				{ok,GID} = rfc4627:get_field(P, "gid"),
				{ok,Domain} = rfc4627:get_field(P, "domain"),
				GID_str = case is_binary(GID) of true -> binary_to_list(GID); _-> GID end,
				Domain_str = case is_binary(Domain) of true -> binary_to_list(Domain); _-> Domain end,
				aa_super_group_chat:reload_group_user(Domain_str,GID_str) ;
	  "reload" when S =:= "group_user" ->
				{ok,P} = rfc4627:get_field(Obj, "params"),
				{ok,GID} = rfc4627:get_field(P, "gid"),
				{ok,Domain} = rfc4627:get_field(P, "domain"),
				GID_str = case is_binary(GID) of true -> binary_to_list(GID); _-> GID end,
				Domain_str = case is_binary(Domain) of true -> binary_to_list(Domain); _-> Domain end,
				aa_group_chat:reload_group_user(Domain_str,GID_str);
	  "reload" when S =:= "mask" ->
				{ok,P} = rfc4627:get_field(Obj, "params"),
				{ok,MASK_FROM} = rfc4627:get_field(P, "from"),
				{ok,MASK_TO} = rfc4627:get_field(P, "to"),
				MASK_FROM_STR = case is_binary(MASK_FROM) of true -> binary_to_list(MASK_FROM); _-> MASK_FROM end,
				MASK_TO_STR = case is_binary(MASK_TO) of true -> binary_to_list(MASK_TO); _-> MASK_TO end,
				aa_packet_filter:reload(mask,MASK_FROM_STR,MASK_TO_STR);
	  "reload_all" when S =:= "mask" ->
				{ok,P} = rfc4627:get_field(Obj, "params"),
				{ok,MASK_TO} = rfc4627:get_field(P, "to"),
				MASK_TO_STR = case is_binary(MASK_TO) of true -> binary_to_list(MASK_TO); _-> MASK_TO end,
				aa_packet_filter:reload_all(mask,MASK_TO_STR);
	  "reload" when S =:= "opt_userlist" ->
				aa_mongodb:set_opt_userlist();
	   "add" when S =:= "be_blocked"->
				{ok,P} = rfc4627:get_field(Obj, "params"),
				{ok,BLOCKEDID_LIST} = rfc4627:get_field(P, "blockedid_list"),
				{ok,TIME_LIST} = rfc4627:get_field(P, "time_list"),				%%time等于0为永久移除
				aa_shielding_system:add_blocked(BLOCKEDID_LIST,TIME_LIST);
	   "remove" when S =:= "be_blocked"->
				{ok,P} = rfc4627:get_field(Obj, "params"),
				{ok,BLOCKEDID_LIST} = rfc4627:get_field(P, "blockedid_list"),
				aa_shielding_system:remove_blocked(BLOCKEDID_LIST);
	  _->
		  skip
  end.


%% @author Administrator
%% @doc @todo Add description to aa_mongodb.


-module(aa_shielding_manage).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("include/ejabberd.hrl").
-include("include/jlib.hrl").
-include("include/logger.hrl").

-define(SYSN_TIME,30*60).			%%同步时间间隔


-define(ERROR_DICT,undefined).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0,sycn_from_other_node/2]).

start_link()->
	gen_server:start_link({local,?MODULE}, ?MODULE, [], []).



%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).

-record(service_userdate,{key,uidlist}).

-define(USERKEY,userkey).

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
  erlang:send(?MODULE,sycn_shielding,[]),
%	{ok,Tref} = timer:send_interval(timer:seconds(?SYSN_TIME), sycn_shielding),
%    {ok, #state{tref = Tref}}.
    {ok, #state{}}.


%%异步存库
sycn_from_other_node(From,To)->
	gen_server:cast(?MODULE, {sycn_other_node,From,To}).
		

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
    Reply = error,
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
handle_cast({sycn_other_node,From,To},State) when is_integer(From) and is_integer(To) ->
	try
		Next = From + 1000,
		if
			Next > To ->
				{ok,Data} = do_read_member_setting(From,To),
				deel_shielding(Data);
			true->
				{ok,Data} = do_read_member_setting(From,Next),
				deel_shielding(Data),
				sycn_other_node(Next+1,To)
  	end
 catch
	 E:R->
	  	?ERROR_MSG("sycn_other node~p",[{E,R}])
  end,
	{noreply,State};  

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
handle_info(sycn_shielding,State)->
	try
		?INFO_MSG("sysn_mongo state:~p",[State]),
		Sql = "SELECT count(*) FROM yue_member_setting",
		{ok,[[Count]]} = aa_hookhandler:mysql_cmd(Sql),
		if
			Count > 1000+1 ->
				{ok,Data} = do_read_member_setting(0,1000),
				deel_shielding(Data),
				sycn_other_node(1001,Count);
			true->
				{ok,Data} = do_read_member_setting(0,Count),
				deel_shielding(Data)
		end
	catch
		E:R->
			erlang:send_after(10000,?MODULE,sycn_shielding),
			?ERROR_MSG("sysn_shielding ~p",[{E,R}])
	end,
	{noreply, State};

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
terminate(_Reason, State) ->
%	{ok, cancel} = timer:cancel(State#state.tref),
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
do_read_member_setting(From,To)->
	Sql1 = "select uid,bantime,types from yue_member_setting where types != '[]' limit ~s,~s",
	Sql = io_lib:format(Sql1,[integer_to_list(From),integer_to_list(To)]),
	aa_hookhandler:mysql_cmd(Sql).

deel_shielding(Data)->
	lists:foreach(fun(D)->
				[Uid,EndTime,Types] = D,
				LLTypes = binary_to_list(Types),
				if
					LLTypes =:=[]->
						skip;
					true->
			    	[_|LeftTypes] = LLTypes,
			    	Type = LeftTypes--"]",
			    	TypeList = string:tokens(Type,","),
			    	CheckAll = erlang:length(TypeList) >= 4,
            if
			    		CheckAll =:= true ->
			    			Key = "be_blocked_user_"++integer_to_list(Uid),
			    			set_cache(Key,EndTime);
			    		true->
			    			lists:foreach(fun(T)->
			    					if
			    						T =:= "normalchat"->
			    							Key = "be_blocked_user_normalchat_"++integer_to_list(Uid),
			    							set_cache(Key,EndTime);
			    						T =:= "groupchat"->
			    							Key = "be_blocked_user_groupchat_"++integer_to_list(Uid),
			    							set_cache(Key,EndTime);
			    						T =:= "super_groupchat"->
			    							Key = "be_blocked_user_super_groupchat_"++integer_to_list(Uid),
			    							set_cache(Key,EndTime);
			    						true->
			    							skip
			    					end
			    				end,TypeList)
			    	end
				end,
				timer:sleep(100),
				?WARNING_MSG("d~p",[D])
		end,Data).

set_cache(Key,EndTime) when is_integer(EndTime) ->
	{M,S,_SS} = now(),
	NowTime = M*1000000+S,
	Cmd = 
  if
	  EndTime =:= 111111 ->
      ["SET",Key,true];
		EndTime =:= 0 ->
			["DEL",Key];
		EndTime =< NowTime ->
			["DEL",Key];
		true->
			["SETEX",Key,EndTime - NowTime,true]
	end,
	aa_hookhandler:ecache_cmd(Cmd);
set_cache(_,_)->
	skip.

sycn_other_node(From,To)->
	try
  	AllNodes = nodes(),
  	Fnodes =
  	lists:filter(fun(N)->
  				[H|_] = string:tokens(atom_to_list(N),"@"),
  				H =:= "ejabberd"
  		end,AllNodes),
  	Enode = [node()|Fnodes],
  	L = length(Enode),
  	UNode = lists:nth(random:uniform(L),Enode),
  	rpc:cast(UNode,aa_shielding_manage,sycn_from_other_node,[From,To])
	catch
		E:R->
			?ERROR_MSG("sycn other node ~p",[{E,R,From,To,erlang:get_stacktrace()}])
	end.

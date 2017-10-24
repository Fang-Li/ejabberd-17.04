-module(aa_log).

-behaviour(gen_server).


-include("include/ejabberd.hrl").
-include("include/jlib.hrl").
-include("include/logger.hrl").


-define(HTTPHead,"application/x-www-form-urlencoded").

%% API
-export([start_link/0,store/1,get_text_message_from_packet/1]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-export([calculation_of_active_days/5,calculat_msg/0,save_to_log/2]).
-compile(export_all).

-record(state, {node}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

store(Packet) ->
	gen_server:cast(aa_log,{store,Packet}).

save_to_log(_Message,_Domain)->
	[].
%	Log_node = ejabberd_config:get_local_option({opt_log_node,Domain}),
%	try
%		case net_adm:ping(Log_node) of
%			pang->
%				?ERROR_MSG("cant conn opt_log_node",[]);
%			pong->
%				rpc:cast(Log_node, opt_log, log_cmd, [Message])
%		end
%	catch
%		_:_->
%			?ERROR_MSG("ERROR save_to_log R:~p",[erlang:get_stacktrace()])
%	end.

init([]) ->
	[Domain|_] = ?MYHOSTS,
	N = ejabberd_config:get_option({log_node,Domain},fun(V)->V end),
	State = #state{node=N},
	{ok,State}.

get_text_message_from_packet( Packet )->
	{xmlel,<<"message">>,_,Message } = Packet,
	List = feach_message(Message,[]),
	?DEBUG("~p ====~n ~p ",[liangc_debug_offline_message,List]),
	List.


feach_message([Element|Message],List) ->
	case Element of
		{xmlel,<<"body">>,_,_} ->
			feach_message(Message,[get_text_message_form_packet_result(Element)|List]);
		_ ->
			feach_message(Message,List)
	end;
feach_message([],List) ->
	List.


get_text_message_form_packet_result( Body )->
       {xmlel,<<"body">>,_,List} = Body, 
       Res = lists:map(fun({_,V})-> binary_to_list(V) end,List),
       ResultMessage = binary_to_list(list_to_binary(Res)),
       ResultMessage.

log(Packet,N) ->
	try
		case Packet of 
			{xmlel,<<"message">>,Attr,_} -> 
				D = dict:from_list(Attr),
				ID      = case dict:is_key(<<"id">>,D) of true-> dict:fetch(<<"id">>,D); false-> "" end,
				From    = case dict:is_key(<<"from">>,D) of true-> dict:fetch(<<"from">>,D); false-> "" end,
				To      = case dict:is_key(<<"to">>,D) of true-> dict:fetch(<<"to">>,D); false-> "" end,
				MsgType = case dict:is_key(<<"msgtype">>,D) of true-> dict:fetch(<<"msgtype">>,D); false-> "" end,
				Msg     = erlang:list_to_binary(get_text_message_from_packet(Packet)),
				Message = {ID,From,To,MsgType,Msg},
				case net_adm:ping(N) of
					pang ->
						?INFO_MSG("write_log ::::> ~p",[Message]),
						Message;
					pong ->
						%% 2014-11-07 : 这里的消息没有过滤，太多造成hiddenNode 频繁死机
						%% 				关闭此功能
						%% {logbox,N}!Message
						?INFO_MSG("write_log ::::> ~p",[Message]) 
				end;
			_ ->
				skip
		end
	catch
		E:I ->
			Err = erlang:get_stacktrace(),
			?ERROR_MSG("write_log_error ::::> E=~p ; I=~p~n Error=~p",[E,I,Err]),
			{error,E,I}
	end.


%% =================================================
%% 计算活跃天数
%% =================================================

calculation_of_active_days(GroupId,Packet,Domain,GroupLevel,Member)->
	{_,_,Attr,_} = Packet,
	case lists:keyfind("msgtype", 1, Attr) of
		{_,"super_groupchat"}->
			case get_level_check_people(GroupLevel,Member) of
				false->
					ok;
				true->
					MsgCount = get_super_group_msg(GroupId,GroupLevel),
					case get_level_check_msg(GroupLevel,MsgCount) of
						true->
							NewLevel = erlang:integer_to_binary(binary_to_integer(GroupLevel)+1),
                            Method = <<"get_group_liveness">>,
							Params =  {[{<<"gid">>,list_to_binary(GroupId)},{<<"level">>,NewLevel}]},
							case aa_packet_filter:call_http(Domain,Method,Params) of
								{ok,_Entity} ->
									clean_new_level(GroupId,NewLevel),
								    aa_super_group_chat:change_level(GroupId,Domain,NewLevel);
								{error,Error} ->
									?ERROR_MSG("calculation_of_active_days error .. ~p",[Error]),
									skip;
							    Exception ->
                                    ?ERROR_MSG("calculation_of_active_days exception .. ~p",[Exception]),
									skip
							end;

						false->
							{Y,M,D} = date(),
							DataValue = integer_to_list(Y)++integer_to_list(M)++integer_to_list(D),
							KEY = "supergroup_active_calculat_"++DataValue++"_"++GroupId,
							SetCmd = ["INCR",KEY],
							aa_hookhandler:ecache_cmd(SetCmd),
							ok
					end
			end;
		_->
			skip
	end.
					

%%计算消息量 是全服消息
calculat_msg()->
	{Y,M,D} = date(),
	DataValue = integer_to_list(Y)++integer_to_list(M)++integer_to_list(D),
	KEY = "calculat_msg_"++DataValue,
	Cmd = ["INCR",KEY],
	aa_hookhandler:ecache_cmd(Cmd).

handle_cast({store,Packet},#state{node=N}=State) ->
	log(Packet,N),
	{noreply, State}.


handle_call(_Request, _From, State) -> Reply = ok, {reply, Reply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%获取当前累计消息数
get_super_group_msg(Groupid,Level)->
	Keys  = get_level_check_key(Groupid,Level),
	lists:foldl(fun(KEY,Acc)->
						GetCmd = ["GET",KEY],
						case aa_hookhandler:ecache_cmd(GetCmd) of
							{ok,undefined} ->
								EX = get_level_check_ex(Level),
								SETEXCMD  = ["SETEX",KEY,EX,0],
								aa_hookhandler:ecache_cmd(SETEXCMD),
								Acc;
							{ok,BMsgNum}->
								MsgNum = 
									try
										erlang:binary_to_integer(BMsgNum)
									catch
										_:_->
											0
									end,
								Acc+MsgNum;
							_->
								0
						end
					end, 0, Keys).

%%升级时清空累计条数
clean_new_level(GroupId,Level)->
	case  get_level_check_key(GroupId,Level) of
		skip->
			skip;
		Keys->
			EX = get_level_check_ex(Level),
			lists:foreach(fun(Key)->
								  	SETEXCMD  = ["SETEX",Key,EX,0],
									aa_hookhandler:ecache_cmd(SETEXCMD)
								  end, Keys)
	end.

%%获取等级对应的统计key存活时间
get_level_check_ex(Level)->
	if
		Level  =:= <<"1">>->
			60*60*24*2;
		Level =:= <<"2">>->
			60*60*24*5;
		Level =:= <<"3">>->
			60*60*24*7;
		Level =:= <<"4">>->
			60*60*24*7;
		true->
			skip
	end.

%%获取等级对应的统计天数
get_level_check_key(GroupId,Level)->
	if
		Level  =:= <<"1">>->
			get_Key_strs(GroupId,[],1);
		Level =:= <<"2">>->
			get_Key_strs(GroupId,[],4);
		Level =:= <<"3">>->
			get_Key_strs(GroupId,[],6);
		Level =:= <<"4">>->
			get_Key_strs(GroupId,[],6);
		true->
			skip
	end.
get_Key_strs(_GroupId,Strs,Day) when Day < 0 ->
	Strs;
get_Key_strs(GroupId,Strs,Day)->
	Time = timestamp()-(60*60*24*Day),
	{{Y,M,D},_} = timestamp_to_datetime(Time),
	DataValue = integer_to_list(Y)++integer_to_list(M)++integer_to_list(D),
	KEY = "supergroup_active_calculat_"++DataValue++"_"++GroupId,
	NStr = [KEY|Strs],
	get_Key_strs(GroupId,NStr,Day-1).
	

%%获取等级对应的统计人数
get_level_check_people(Level,Member)->
	if
		Level  =:= <<"1">>->
			Member >= 22;
		Level =:= <<"2">>->
			Member >= 40;
		Level =:= <<"3">>->
			Member >= 80;
		Level =:= <<"4">>->
			Member >= 180;
		true->
			false
	end.

%%获取等级对应的统计消息数
get_level_check_msg(Level,MsgCount)->
	if
		Level  =:= <<"1">>->
			MsgCount >= 200;
		Level =:= <<"2">>->
			MsgCount >= 500;
		Level =:= <<"3">>->
			MsgCount >= 1200;
		Level =:= <<"4">>->
			MsgCount >= 2000;
		true->
			false
	end.
%%时间戳转换成日期
timestamp_to_datetime(Timestamp) ->
    calendar:gregorian_seconds_to_datetime(Timestamp +
      calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}})).

%%时间戳
timestamp() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.


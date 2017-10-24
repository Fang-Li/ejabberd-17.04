%% @author Administrator
%% @doc @todo Add description to aa_mongodb.


-module(aa_mongodb).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("include/ejabberd.hrl").
-include("include/jlib.hrl").
-include("include/logger.hrl").

-define(OPERATE_USERLIST,"operate_userlist").

-define(SYSN_TIME,30).            %%同步时间间隔

-define(MONGO_POOL,mongo_pool).

-define(ERROR_DICT,undefined).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0,
        save_mongo/3,
        save_super_group_mongo/4,
        get_userlist/0
        ]).

start_link()->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).



%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {tref}).

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
    UserList = 
        try
            lists:filter(fun(Uid)-> erlang:is_integer(Uid) end, aa_mongo_senter:read_opt_uidlist())
        catch
            _:_->
                []
        end,
    case ets:info(service_userdate) of
        ?ERROR_DICT->
            ets:new(service_userdate, [named_table, set, public,{keypos,#service_userdate.key}]),
            ok;
        _->
             ok
    end,
    ets:insert(service_userdate,#service_userdate{key = ?USERKEY,uidlist = UserList}),
    {ok,Tref} = timer:send_interval(timer:seconds(?SYSN_TIME), read_mongouser),%%同步mongo数据库中的运营账户
    ?WARNING_MSG("sysn_mongo state:~p",[UserList]),
    {ok, #state{tref = Tref}}.


%%异步存库
save_mongo(From, To, Packet)->
    case Packet of 
        {_,<<"message">>,_Attr,_} ->
            gen_server:cast(?MODULE, {save_mongo,From, To,Packet});
        _->
            skip
    end.

save_super_group_mongo(From,To,Packet,GroupData)->
    case Packet of
        {_,<<"message">>,_,_}->
            gen_server:cast(?MODULE, {save_super_group_mongo,From, To,Packet,GroupData});
        _->
            skip
    end.

get_userlist()->
     case ets:lookup(service_userdate, ?USERKEY) of
           [#service_userdate{uidlist = L}]->
               L;
           _->
               []
       end.
        

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
handle_cast({save_mongo,#jid{luser = Fromstr, lserver = FDomain} = FromJid, #jid{luser = Tostr, lserver = TDomain} = ToJid,Packet},State)->
    Isoffline =  fxml:get_tag_attr_s(<<"isoff">>, Packet),
  MsgType = fxml:get_tag_attr_s(<<"msgtype">>, Packet),
  Fenable = aa_config:enable_planning_system(FDomain) or (MsgType =:= <<"system">>), 
  Tenable = aa_config:enable_planning_system(TDomain) or (MsgType =:= <<"system">>), 
    if
    Fenable =/= true;Tenable=/=true ->
      skip;
        Isoffline =:= <<"1">>;Fromstr =:= <<"stranger_limit">> ->
            skip;
        true->
            Mid = fxml:get_tag_attr_s(<<"id">>, Packet),
            Type = fxml:get_tag_attr_s(<<"type">>, Packet),
            OldMsgTime = fxml:get_tag_attr_s(<<"msgTime">>, Packet),
            
            if
                OldMsgTime =/= <<"">> ->
                    MsgTime = OldMsgTime;
                true->
                    {M,S,SS} = now(), 
                    MsgTime = list_to_binary(lists:sublist(erlang:integer_to_list(M*1000000000000+S*1000000+SS),1,13))
            end,
            try
                case MsgType of
                    <<"normalchat">> ->
                        From = erlang:binary_to_integer(Fromstr),
                        To = erlang:binary_to_integer(Tostr),
                        Userlist = case ets:lookup(service_userdate, ?USERKEY) of
                                       [#service_userdate{uidlist = L}]->
                                           L;
                                       _->
                                           []
                                   end,
                        Result =  lists:member(To, Userlist),
                        FResult = lists:member(From, Userlist),
                        if
                            Result =:= false,FResult =:= false->
                                skip;
                            true->
                                [JSON] = aa_log:get_text_message_from_packet(Packet),
                                StructJson = jiffy:decode(list_to_binary(JSON)),
                                Body = normal_deel_json_to_bson(StructJson),
                                DbMsg = {
                                         id,list_to_binary(Mid),
                                         from,From,
                                         to,To,
                                         type,list_to_binary(Type),
                                         msgtype,list_to_binary(MsgType),
                                         msgTime,erlang:binary_to_integer(MsgTime),
                                         body,Body,
                                         isread,[]
                                         },
                                aa_mongo_senter:sysn_write([DbMsg])
                        end;
                    <<"system">> ->
                        To = erlang:binary_to_integer(Tostr),
                        From = erlang:binary_to_integer(Fromstr),
                        IsToGroup = aa_group_chat:is_group_chat(ToJid),
                        IsFromGroup  = aa_group_chat:is_group_chat(FromJid),
                        if
                            IsToGroup=:=false,IsFromGroup=:=false->
                                deel_system_msg(To,From,Tostr,Fromstr,Packet,Mid,Type,MsgType,MsgTime);
                            IsToGroup=:=true,IsFromGroup=:=false->
                                deel_system_msg(To,From,"skip",Fromstr,Packet,Mid,Type,MsgType,MsgTime);
                            IsToGroup=:=false,IsFromGroup=:=true->
                                deel_system_msg(To,From,Tostr,"skip",Packet,Mid,Type,MsgType,MsgTime);
                            true->
                                skip
                        end;
                    _->
                        skip
                end
            catch
                E:R->
                    ?ERROR_MSG("Error happen at ~p :save_mongo()reason:~p",[?MODULE,{E,R,erlang:get_stacktrace()}])
            end
    end,
    {noreply, State};

handle_cast({save_super_group_mongo,#jid{luser = Fromstr} = _FromJid, #jid{luser = Tostr} = _ToJid,Packet,{GroupId,Groupname,GroupImage,UserList}},State)->
    Mid = fxml:get_tag_attr_s(<<"id">>, Packet),
    Type = fxml:get_tag_attr_s(<<"type">>, Packet),
    MsgType = fxml:get_tag_attr_s(<<"msgtype">>, Packet),
    OldMsgTime = fxml:get_tag_attr_s(<<"msgTime">>, Packet),
    if
        OldMsgTime =/= <<"">>->
            MsgTime = OldMsgTime;
        true->
            {M,S,SS} = now(), 
            MsgTime = list_to_binary(lists:sublist(erlang:integer_to_list(M*1000000000000+S*1000000+SS),1,13))
    end,
    try
        case MsgType of
            <<"super_groupchat">> ->
                From = erlang:binary_to_integer(Fromstr),
                To = erlang:binary_to_integer(Tostr),
                ServiceUserlist = 
                    case ets:lookup(service_userdate, ?USERKEY) of
                       [#service_userdate{uidlist = L}]->
                           L;
                       _->
                           []
                   end,
                GroupUserlist = [Fromstr|UserList],
                {Result,Service_group_user} = 
                    lists:foldl(fun(U,Acc)->
                                        {_,AccData} = Acc,
                                        case lists:member(erlang:integer_to_binary(U),GroupUserlist) of
                                                true->
                                                    {true,[U|AccData]};
                                                _->
                                                    Acc
                                        end
                                    end, {false,[]}, ServiceUserlist),
                if
                    Result =:= false->
                        skip;
                    true->
                        [PJSON] = aa_log:get_text_message_from_packet(Packet),
                        StructJson = jiffy:decode(erlang:list_to_binary(PJSON)),
                        J1 = ej:set({<<"groupname">>},StructJson,Groupname),
                        J2 = ej:set({<<"groupimage">>},J1,GroupImage),
                        J3 = ej:set({<<"groupid">>},J2,list_to_binary(GroupId)),
                        Body = normal_deel_json_to_bson(J3),
                        DbMsg = {
                                 id,list_to_binary(Mid),
                                 from,From,
                                 to,To,
                                 type,list_to_binary(Type),
                                 msgtype,list_to_binary(MsgType),
                                 servicer_in_group,Service_group_user,
                                 msgTime,erlang:binary_to_integer(MsgTime),
                                 body,Body,
                                 isread,[]
                                 },
                        aa_mongo_senter:sysn_write([DbMsg])
                end;
            _->
                skip
        end
    catch
        E:R->
            ?ERROR_MSG("Error happen at ~p :save_super_group_mongo()reason:~p",[?MODULE,{E,R,erlang:get_stacktrace()}])
    end,
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
handle_info(read_mongouser,State)->
    ?INFO_MSG("sysn_mongo state:~p",[State]),
    UserList = lists:filter(fun(Uid)-> erlang:is_integer(Uid) end, aa_mongo_senter:read_opt_uidlist()),
%%     NewState = State#state{userlist = UserList},
    ets:insert(service_userdate, #service_userdate{key = ?USERKEY,uidlist = UserList}),
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
    {ok, cancel} = timer:cancel(State#state.tref),
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

%%他们要的系统消息
deel_system_msg(Tointeger,Frominteger,To,From,Packet,Mid,MType,MsgType,MsgTime)->
    ServiceUserlist = 
        case ets:lookup(service_userdate, ?USERKEY) of
           [#service_userdate{uidlist = L}]->
               L;
           _->
               []
       end,
    Uselist = lists:map(fun(H)->erlang:integer_to_list(H) end, ServiceUserlist),
    case lists:member(To,Uselist) or lists:member(From,Uselist) of
        true->
            [JSON] = aa_log:get_text_message_from_packet(Packet),
            StructJson = jiffy:decode(erlang:list_to_binary(JSON)),
            case ej:get({<<"type">>},StructJson) of 
                    undefined ->
                        skip;
                    Type ->
                        if
                            Type =:= <<"0">>;Type =:= <<"101">>;Type =:= <<"104">>->
                                Body = normal_deel_json_to_bson(StructJson),
                                DbMsg = {
                                             id,list_to_binary(Mid),
                                             from,Frominteger,
                                             to,Tointeger,
                                             type,list_to_binary(MType),
                                             msgtype,list_to_binary(MsgType),
                                             msgTime,erlang:binary_to_integer(MsgTime),
                                             body,Body,
                                             isread,[]
                                             },
                                aa_mongo_senter:sysn_write([DbMsg]);
                            true->
                                skip
                        end
            end;
        _->
            skip
    end.


%% =================================================
%% 替换json为bson：key替换为atom 数字格式全部为数字
%% =================================================
             
normal_deel_json_to_bson(StructJson)->
    Ljson = case ej:get({<<"type">>},StructJson) of
      undefined ->
          StructJson;
      Type ->
          ej:set({<<"type">>},StructJson,type_util:to_integer(Type))
    end,
    
    Ljson2 = ej:delete({<<"cover">>},Ljson),
    

    Tsonlist = 
        case ej:get({<<"groupmember">>},Ljson2) of
            undefined ->
                Ljson2;
            GroupMemberList ->
                NewGrm = 
                lists:map(fun({UserInfo})->
                            list_to_tuple(lists:append(lists:map(fun({K,V})->[type_util:to_atom(K),V] end, UserInfo)))
                          end, GroupMemberList),
                ej:set({<<"groupmember">>},Ljson2,NewGrm)
        end,
    {Proplist} = Tsonlist,
    list_to_tuple(lists:append(lists:map(fun({K,V})->[type_util:to_atom(K),V] end, Proplist))).


test_normal_deel_json_to_bson() ->
  %% {struct,[{<<"mask">>,<<"0">>},
  %%             {<<"groupname">>,
  %%              <<74,105,109,227,128,129,108,101,111,110,227,128,129,232,
  %%                131,161,232,131,161>>},
  %%             {<<"groupmember">>,
  %%              [{struct,[{<<"uid">>,<<"15538">>},
  %%                        {<<"gender">>,<<"1">>},
  %%                        {<<"image">>,
  %%                         <<"http://beta.iyueni.com/Uploads/avatar/2/15538_pgL9AK.jpg"...>>}]},
  %%               {struct,[{<<"uid">>,<<"15475">>},
  %%                        {<<"gender">>,<<"1">>},
  %%                        {<<"image">>,
  %%                         <<"http://beta.iyueni.com/Uploads/avatar/2/15475_2VN7uq"...>>}]},
  %%               {struct,[{<<"uid">>,<<"15597">>},
  %%                        {<<"gender">>,<<"1">>},
  %%                        {<<"image">>,
  %%                         <<"http://beta.iyueni.com/Uploads/avatar/2/15597_Ft"...>>}]}]},
  %%             {<<"groupid">>,<<"394">>},
  %%             {<<"userid">>,<<"15475">>},
  %%             {<<"username">>,<<"leon">>},
  %%             {<<"userimage">>,
  %%              <<"http://beta.iyueni.com/Uploads/avatar/2/15475_2VN7uq.jpg_200_200_2_80.jp"...>>},
  %%             {<<"usergender">>,1},
  %%             {<<"type">>,104},
  %%             {<<"content">>,<<"1">>},
  %%             {<<"msgsource">>,<<231,186,166,228,189,160>>},
  %%             {<<"usersource">>,<<"yuejian.net">>},
  %%             {<<"userfrom">>,<<231,186,166,228,189,160>>},
  %%             {<<"msgtype">>,<<"0">>}]}
  
  %% ==========> 转为 
  
  %% {mask,<<"0">>,groupname,
  %%       <<74,105,109,227,128,129,108,101,111,110,227,128,129,232,
  %%         131,161,232,131,161>>,
  %%       groupmember,
  %%       [{uid,<<"15538">>,gender,<<"1">>,image,
  %%             <<"http://beta.iyueni.com/Uploads/avatar/2/15538_pgL9AK.jpg_200_200"...>>},
  %%        {uid,<<"15475">>,gender,<<"1">>,image,
  %%             <<"http://beta.iyueni.com/Uploads/avatar/2/15475_2VN7uq.jpg_200"...>>},
  %%        {uid,<<"15597">>,gender,<<"1">>,image,
  %%             <<"http://beta.iyueni.com/Uploads/avatar/2/15597_FtussD.jpg"...>>}],
  %%       groupid,<<"394">>,userid,<<"15475">>,username,<<"leon">>,
  %%       userimage,
  %%       <<"http://beta.iyueni.com/Uploads/avatar/2/15475_2VN7uq.jpg_200"...>>,
  %%       usergender,1,type,104,content,<<"1">>,msgsource,
  %%       <<231,186,166,228,189,160>>,
  %%       usersource,<<"yuejian.net">>,userfrom,
  %%       <<231,186,166,...>>,
  %%       msgtype,<<"0">>}
  ok.

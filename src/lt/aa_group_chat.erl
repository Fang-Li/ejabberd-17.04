-module(aa_group_chat).

-include("include/ejabberd.hrl").
-include("include/jlib.hrl").
-include("include/logger.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([reload_group_user/2]).

-define(HTTP_HEAD,"application/x-www-form-urlencoded").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
    start_link/0,
    route_group_msg/3,
    is_group_chat/1,
    check_group_type/1
]).

-record(state, {}).

start() ->
    aa_group_chat_sup:start_child().

stop(Pid)->
    gen_server:cast(Pid,stop).

start_link() ->
    gen_server:start_link(?MODULE,[],[]).

route_group_msg(From,To,Packet)->
    {ok,Pid} = start(),
    ?DEBUG("###### route_group_msg_001 ::::> {From,To,Packet}=~p",[{From,To,Packet}]),
    gen_server:cast(Pid,{route_group_msg,From,To,Packet}),
    ?DEBUG("###### route_group_msg_002 ::::> {From,To,Packet}=~p",[{From,To,Packet}]).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
init([]) ->
    {ok,#state{}}.

handle_cast({route_group_msg,From,To,Packet}, State) ->
    try
        handle_call({route_group_msg,From,To,Packet},[],State) 
    catch 
        _:_ ->
            Err = erlang:get_stacktrace(),
            ?ERROR_MSG("route_group_msg_error ~p",[Err])
    end,
    {stop, normal, State};
handle_cast(stop, State) ->
    {stop, normal, State}.

%% ==========================================
%% 通知用户自己讨论组已经解散了，本条消息无法发送了
%% ==========================================

notify_group_disbanded(#jid{user=FromUser,server=_FromDomain}=From,#jid{user=GroupId,server=GDomain}=To,Packet,[TO_ID,FDomain,NewTo]) ->
    %% The exit or dissociation group method is used to notify the user himself
    %%TODO 解散了
    %% <message id="xxxxx" from="yy@group.yuejian.net" to="123456@yuejian.net" type="normal" msgtype=“system”>
    %%  <body>{groupid":"xx","groupname":"",groupmember":[],"type":"15"}</body>
    %% </message>

    {X,E,Attr,_} = Packet,
    RAttr = lists:map(fun({K,V})->
      case K of
        <<"id">>->          {K,TO_ID};
        <<"from">> ->       {K,<<GroupId/binary,"@",GDomain/binary>>};
        <<"to">> ->         {K,<<FromUser/binary,"@",FDomain/binary>>};
        <<"type">> ->       {K,<<"normal">>};
        <<"msgtype">> ->    {K,<<"system">>}; 
        _ ->            {K,V} 
      end
    end,Attr),
    J1 = {[{<<"groupid">>,list_to_binary(GroupId)},
           {<<"groupname">>,<<"">>},
           {<<"groupmember">>,[]},
           {<<"type">>,<<"15">>}
                 ]},
    ExitJson = jiffy:encode(J1),
    Body = [{xmlel,<<"body">>,[],[{xmlcdata,ExitJson}]}],
    RPacket = {X,E,RAttr,Body},
    %% 20141223 : 这里不能再用 otp ，瓶颈
    % ?ERROR_MSG("1 .......................   ~p",[]),
    aa_hookhandler:handle_cast({group_chat_filter,From,NewTo,RPacket,false},#state{}),
    case ejabberd_router:route(To,From,RPacket) of
      ok ->
        %% gen_server:cast(aa_hookhandler,{group_chat_filter,From,To,RPacket,false}),
        ?DEBUG("###### route_group_type15 OK :::> {From,To,RPacket}=~p",[{To,From,RPacket}]),
        {ok,ok};
      Err ->
        ?DEBUG("###### route_group_type15 ERR=~p :::> {From,To,RPacket}=~p",[Err,{To,From,RPacket}]),
        {error,Err}
    end.
%% =================================================
%% 通知用户已经被踢出了讨论组
%% =================================================

notify_out_group(#jid{user=FromUser,server=_FromDomain}=From,#jid{user=GroupId,server=GDomain}=To,Packet,[TO_ID,FDomain,NewTo, Groupname,Groupmember]) ->
    %%TODO 被T了
    %% <message id="xxxxx" from="1@yuejian.net" to"yy@yuejian.net" type="normal" msgtype=“system”>
    %%       <body>{groupid":"xx","groupname":"...","groupmember":[...],"type":"14"}</body>
    %% </message>
    {X,E,Attr,_} = Packet,
    RAttr = lists:map(fun({K,V})->
      case K of
        <<"id">>->                    {K,TO_ID};
        <<"from">> ->                 {K,<<GroupId/binary,"@",GDomain/binary>>};
        <<"to">> ->                   {K,<<FromUser,"@",FDomain/binary>>};
        <<"type">> ->                 {K,<<"normal">>};
        <<"msgtype">> ->              {K,<<"system">>};
        _ ->                      {K,V} 
      end
    end,Attr),
    StructJson = {[{<<"groupid">>,GroupId},
             {<<"groupname">>,Groupname},
             {<<"groupmember">>,Groupmember},
             {<<"type">>,<<"14">>}
             ]},
    Json = jiffy:encode(StructJson),
    Body = [{xmlel,<<"body">>,[],[{xmlcdata,Json}]}],
    RPacket = {X,E,RAttr,Body},
    %% 20141223 : 这里不能再用 otp ，瓶颈
    aa_hookhandler:handle_cast({group_chat_filter,From,NewTo,RPacket,false},#state{}),
    case ejabberd_router:route(To,From,RPacket) of
      ok ->
        ?DEBUG("route_group_type14 OK :::> {From,To,RPacket}=~p",[{To,From,RPacket}]);
        %% gen_server:cast(aa_hookhandler,{group_chat_filter,From,To,RPacket,false});
      Err ->
        ?DEBUG("route_group_type14 ERR=~p :::> {From,To,RPacket}=~p",[Err,{To,From,RPacket}]) 
    end.

%% 这个地方是用 cast 代理调用的，不要用 call 来调用，当时的笔误
handle_call({route_group_msg,#jid{user=FromUser,server=FDomain}=From,#jid{user=GroupId,server=_GDomain}=To,Packet}, _From, State) ->
    MID= fxml:get_tag_attr_s(<<"id">>, Packet),
    Domain = get_group_host_domain(FDomain),
    {M,S,SS} = now(),
    TO_ID = erlang:integer_to_binary(M*1000000000000+S*1000000+SS),
    NewTo = #jid{user= FromUser, server = Domain, resource = [], luser = FromUser, lserver = Domain, lresource = []},
    
    case get_user_list_by_group_id(Domain,GroupId) of 
        {not_found,_,_,_,_} ->
            notify_group_disbanded(From,To,Packet,[TO_ID,FDomain,NewTo]);
        {ok,UserList,Groupmember,Groupname,Masklist} ->
            case UserList of 
                [] ->
                    notify_group_disbanded(From,To,Packet,[TO_ID,FDomain,NewTo]);
                _ ->
                    case lists:member(FromUser,UserList) of
                        false ->
                            notify_out_group(From,To,Packet,[TO_ID,FDomain,NewTo, Groupname,Groupmember]);
                        true ->    
                            Roster = lists:map(fun(User)-> 
                                UID = User,
                                #jid{user=UID,server= Domain,luser=UID,lserver= Domain,resource=[],lresource=[]} 
                            end,UserList),
                            ?DEBUG("###### route_group_msg 002 :::> GroupId=~p ; Roster=~p",[GroupId,Roster]),
                            ?DEBUG("group_message_title_~p src_msg_id=~p ; roster_size=~p",[GroupId,MID,length(Roster)]),
                            lists:foreach(fun(Target)-> 
                                route_msg(From,Target,Packet,GroupId,Groupmember,Groupname,Masklist,MID) 
                            end,Roster) ,
                            aa_log:save_to_log({group,From,To,Packet,{GroupId,Groupname}}, Domain)
                    end
            end;
        Err ->
            ?ERROR_MSG("group_msg_error ~p",Err),
            error
    end,    
    {reply,[],State}.


handle_info({cmd,_Args},State)->
    {noreply,State}.

terminate(_Reason,_State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

get_group_host_domain(Domain)->
    DomainTokens = str:tokens(Domain,<<".">>),
    case DomainTokens of
        [_,D1,S2] ->
            <<D1/binary,".",S2/binary>>;
        _->
            Domain
    end.

reload_group_user(Domain,GroupId) ->
    Response = get_user_list_by_group_id(do,Domain,GroupId),
    Group_cache_key = <<GroupId/binary,"@",Domain/binary,"/group_cache">>,
    case Response of 
        {ok,[],[],[],[]} ->
            skip;
        {not_found,[],[],[],[]} ->
            %% gen_server:call(aa_hookhandler,{ecache_cmd,["DEL",Group_cache_key]});
            %% 20141115: 防止在一个进程排队，产生瓶颈，特此修正
            aa_hookhandler:ecache_cmd(["DEL",Group_cache_key]);
        {ok,_,_,_,_} ->
            %% gen_server:call(aa_hookhandler,{ecache_cmd,["SET",Group_cache_key,erlang:term_to_binary(Response)]});
            %% 20141115: 防止在一个进程排队，产生瓶颈，特此修正
            aa_hookhandler:ecache_cmd(["SET",Group_cache_key,erlang:term_to_binary(Response)]);
        _ ->
            skip
    end,
    Response.

get_user_list_by_group_id(Domain,GroupId)->
    case GroupId of 
        "cctest" ->
            {ok,[<<"cc1">>,<<"cc2">>,<<"cc3">>],[],[],[]};
        _ ->
             case ejabberd_config:get_option({group_cache_enable,Domain},fun(V)->V end) of 
                true ->
                    get_user_list_by_group_id(cache,Domain,GroupId);
                _ ->
                    get_user_list_by_group_id(do,Domain,GroupId)
            end
    end.
get_user_list_by_group_id(cache,Domain,GroupId) ->
    Group_cache_key = <<GroupId/binary,"@",Domain/binary,"/group_cache">>,
    %% case gen_server:call(aa_hookhandler,{ecache_cmd,["GET",Group_cache_key]}) of
    %% 20141115: 防止在一个进程排队，产生瓶颈，特此修正
    case aa_hookhandler:ecache_cmd(["GET",Group_cache_key]) of
        {ok,Bin} when erlang:is_binary(Bin) ->
            erlang:binary_to_term(Bin);
        _ ->
            reload_group_user(Domain,GroupId)
    end;
get_user_list_by_group_id(do,Domain,GroupId) when is_binary(Domain) ->
    get_user_list_by_group_id(do,binary_to_list(Domain),GroupId);
get_user_list_by_group_id(do,Domain,GroupId) when is_list(Domain) ->
    ?DEBUG("###### get_user_list_by_group_id :::> Domain=~p ; GroupId=~p",[Domain,GroupId]),
    GroupId_bin = case is_binary(GroupId) of 
        true -> 
            GroupId ; 
        _->
            list_to_binary(GroupId)
    end,

    Method = <<"getUserList">>,
    Params = {[{<<"groupId">>,GroupId_bin}]} ,
    case aa_packet_filter:call_http(Domain,Method,Params) of
      {ok,Entity} ->
        UserList = ej:get({<<"userlist">>},Entity),
        Groupmember = ej:get({<<"groupmember">>},Entity),
        Masklist    = ej:get({<<"masklist"   >>},Entity),
        Groupname   = ej:get({<<"groupname"  >>},Entity),
        {ok,UserList,Groupmember,Groupname,Masklist};
      {error,Error} ->
        ?ERROR_MSG("no group member error .. ~p",[Error]),
        {not_found,[],[],[],[]};
      Exception ->
        ?ERROR_MSG("no group member exception .. ~p",[Exception]),
        {ok,[],[],[],[]}
    end.

get_msgtype(Attr) ->
    D = dict:from_list(Attr),
    _MT = case dict:is_key(<<"msgtype">>,D) of 
        true-> 
            case dict:fetch(<<"msgtype">>,D) of
                <<"system">> ->
                    <<"system">>;
                _ ->
                    <<"groupchat">>
            end;
        _-> <<"groupchat">> 
    end.
route_msg(#jid{user=FromUser}=From,#jid{user=User,server=Domain}=To,Packet,GroupId,Groupmember,Groupname,Masklist,MID) ->
    case FromUser=/=User of
        true->
            {X,E,Attr,_Body} = Packet,
            {M,S,SS} = now(),
            MsgTime = list_to_binary(lists:sublist(erlang:integer_to_list(M*1000000000000+S*1000000+SS),1,13)),
            MT = get_msgtype(Attr),
            TO_ID = erlang:integer_to_binary(M*1000000000000+S*1000000+SS),
            Mask = case lists:member(User,Masklist) of true -> <<"1">>; false-> <<"0">> end,
            % -------------------------------------------------
            % 特殊处理隐藏消息，添加消息新id
            % -------------------------------------------------
            RAttr0 = lists:map(fun({K,V})-> 
                case K of 
                    <<"id">> -> 
                        if
                            Mask =:= <<"1">> ->
                                {K,<<"hide_msg_",TO_ID/binary>>};
                            true->
                                {K,TO_ID}
                        end;
                    <<"to">> -> {K,<<User/binary,"@",Domain/binary>>};
                    <<"msgtype">> -> {K,MT};    
                    <<"msgTime">> -> skip;
                    _-> {K,V} 
                end 
            end,Attr),
            RAttr1 = [{<<"mask">>,Mask},{<<"groupid">>,type_util:to_binary(GroupId)}|RAttr0],
            RAttr2 = lists:append([Kv||Kv<-RAttr1,Kv=/=skip],[{<<"msgTime">>,MsgTime}]),

            %% TODO Groupmember
            [JSON] = aa_log:get_text_message_from_packet(Packet),    
            StructJson = jiffy:decode(erlang:list_to_binary(JSON)),
            J1 = ej:set({<<"groupmember">>},StructJson, Groupmember),
            J2 = ej:set({<<"groupname">>},J1, Groupname),
            RJ0 = ej:set({<<"mask">>},J2, Mask),
            J4B = jiffy:encode(RJ0),
            ?DEBUG("GROUP ::::> J4B=~p",[J4B]),
            RBody = [{xmlel,<<"body">>,[],[{xmlcdata,J4B}]}],
            RPacket = {X,E,RAttr2,RBody},
            %% 20141223 : 这里不能再用 otp ，瓶颈
            aa_hookhandler:handle_cast({group_chat_filter,From,To,RPacket,false},#state{}),
            case ejabberd_router:route(From, To, RPacket) of
                ok ->
                    ?DEBUG("group_message_~p src_msg_id=~p ; target_msg_id=~p ; from_user=~p ; to_user=~p ; body=~s",
                                                [GroupId,MID,TO_ID,FromUser,User,J4B]),
                    ?DEBUG("###### route_group_msg 003 OK :::> {From,To,RPacket}=~p",[{From,To,RPacket}]),
                    %% gen_server:cast(aa_hookhandler,{group_chat_filter,From,To,RPacket,false}),
                    {ok,ok};
                Err ->
                    ?DEBUG("###### route_group_msg 003 ERR=~p :::> {From,To,RPacket}=~p",[Err,{From,To,RPacket}]),
                    {error,Err}
            end;
        _ ->
            {ok,skip}
    end.

is_group_chat(#jid{server=Domain}=_To)->
    [GroupName|_] = str:tokens(Domain,<<".">>),
    case GroupName of 
      <<"group">> ->
          true;
      <<"super_group">> ->
          true ;
      _  ->
          false
    end.

check_group_type(Domain) ->
    DomainTokens = str:tokens(Domain,<<".">>),
    [G|_] = DomainTokens,
    GroupType =
        if
            G =:= <<"group">> ->
                <<"group">>;
            G =:= <<"super_group">> ->
                <<"super_group">>;
            true->
                false
        end,
    ?DEBUG("group type is .. ~p",[GroupType]),
    GroupType.

        

-module(aa_super_group_chat).

-include("include/ejabberd.hrl").
-include("include/jlib.hrl").
-include("include/logger.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([reload_group_user/2]).
-compile(export_all).


-define(HTTP_HEAD,"application/x-www-form-urlencoded").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
  start_link/0,
  route_group_msg/3,
  is_group_chat/1,
  get_itdoc_str/3,
  set_special_group_data/4,
  change_level/3,
  set_super_group_msgsource/2
]).

-record(state, {}).

start() ->
  aa_super_group_chat_sup:start_child().

stop(Pid)->
  gen_server:cast(Pid,stop).

start_link() ->
  gen_server:start_link(?MODULE,[],[]).

route_group_msg(From,To,Packet)->
  {ok,Pid} = start(),
  ?DEBUG("###### route_group_msg_001 ::::> ~n  From=~p,~n  To=~p,~n  Packet=~p",[From,To,Packet]),
  gen_server:cast(Pid,{route_group_msg,From,To,Packet}),
  ?DEBUG("###### route_group_msg_002 ::::> ~n  From=~p,~n  To=~p,~n  Packet=~p",[From,To,Packet]).


%% =================================================
%% 添加群组名称、免打扰列表、群组头像
%% =================================================

set_special_group_data(GroupDomain, #jid{luser = Touser, lserver = ToDomain},Packet,StructJson)->
  {X,E,Attr,_} = Packet,
  Domain = get_group_host_domain(GroupDomain),
  %%  [JSON] = aa_log:get_text_message_from_packet(Packet), 
  %%  {ok,JO,_} = rfc4627:decode(erlang:list_to_binary(JSON)),
  GroupId = ej:get({<<"groupid">>},StructJson),
  case get_user_list_by_group_id(Domain,GroupId) of 
    {ok,_UserList,Groupname,Masklist,GroupImage,_GroupLevel,_Members} ->
          {M,S,SS} = now(),
          MsgTime = list_to_binary(lists:sublist(erlang:integer_to_list(M*1000000000000+S*1000000+SS),1,13)),
          %% lifang update 很恶心的代码,以为自己修改了一个bug，谁知道IOS客户端添加一个mask就不认识了
          %% 可之前的分支永远都不会走到这里的啊，写这么个代码干嘛
          Mask = case lists:member({type_util:to_binary(ToDomain), type_util:to_binary(Touser)},Masklist) of true -> <<"1">>; false-> <<"0">> end,
          % -------------------------------------------------
          % 去除value为skip的key_value字段
          % -------------------------------------------------
          RAttr1 = [Kv||Kv<-Attr,Kv=/=skip],
          RAttr2 = [{<<"groupid">>,GroupId},{<<"mask">>,Mask},{<<"msgTime">>,MsgTime}|RAttr1],
          %% lifang update 很恶心的代码,以为自己修改了一个bug，谁知道IOS客户端添加一个mask就不认识了
          %% 可之前的分支永远都不会走到这里的啊，写这么个代码干嘛
          %% JO1 = ej:set({<<"mask">>},StructJson, list_to_binary(Mask)),
          JO1 = ej:set({<<"groupname">>},StructJson, Groupname),
          JO2 = ej:set({<<"groupimage">>},JO1, GroupImage),
          J4B = jiffy:encode(JO2),
          Body = [{xmlel,<<"body">>,[],[{xmlcdata,J4B}]}],
          {{X,E,RAttr2,Body},J4B};
    _->
      J4B = jiffy:encode(StructJson),
      {Packet,J4B}
  end.

change_level(GroupId,Domain,NewLevel)->
  case get_user_list_by_group_id(Domain,GroupId) of
    {ok,UserList,Groupname,Masklist,GroupImage,_GroupLevel,Members}->
      Group_cache_key = <<GroupId/binary,"@",Domain/binary,"/super_group_cache">>,
      aa_hookhandler:ecache_cmd(["SET",Group_cache_key,erlang:term_to_binary({ok,UserList,Groupname,Masklist,GroupImage,NewLevel,Members})]);
    _->
      skip
  end.

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
init([]) ->
  {ok,#state{}}.
display(_,_,_) -> ok.
handle_cast({route_group_msg,From,To,Packet}, State) ->
 %% test function
  case Packet of 
  %% xmlcdata可能会有并列格式
  {xmlel,<<"message">>,_Attrs,[{xmlel,<<"body">>,_Attrs2,[{xmlcdata,JB}]}]} -> 
    display(From,To,JB);
  _ ->
   display(a,b,c) 
  end,
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

%% ===============================================================
%% 群组消息发送处理
%% 单条从这里分散成发送向每个群组用户
%% 并做好是否是群内成员控制
%% 原著者注释：这个地方是用 cast 代理调用的，不要用 call 来调用，当时的笔误
%% ===============================================================

handle_call({route_group_msg,#jid{user=FromUser2,server=FromDomain2}=From,#jid{user=GroupId2,server=GDomain2}=To,Packet}, _From, State) ->
  MID= fxml:get_tag_attr_s(<<"id">>, Packet),
  FromUser = type_util:to_list(FromUser2),
  FromDomain = type_util:to_list(FromDomain2),
  GroupId = type_util:to_list(GroupId2),

  Domain2 = get_group_host_domain(GDomain2),
  Domain =  type_util:to_list(Domain2),
  FDomain2 = get_group_host_domain(FromDomain2),
  FDomain =  type_util:to_list(FDomain2),
  {M,S,SS} = now(),
  TO_ID = erlang:integer_to_binary(M*1000000000000+S*1000000+SS),
  NewTo = #jid{user= FromUser2, server = Domain2, resource = [], luser = FromUser2, lserver = Domain2, lresource = []},

  case get_user_list_by_group_id(Domain,GroupId) of 
    % -------------------------------------------------
    % 不再通知群组解散的消息，静默处理
    % -------------------------------------------------
    {not_found,_,_,_,_,_,_} ->
      % notify_group_disbanded(From,To,Packet,[TO_ID,FDomain,NewTo]),
      ?ERROR_MSG("group user list is not found .. ~p",["not notify_group_disbanded"]);
    {ok,UserList,Groupname,Masklist,GroupImage,GroupLevel,Members} ->
      case UserList of 
        [] ->
          % notify_group_disbanded(From,To,Packet,[TO_ID,FDomain,NewTo]),
          % notify_group_disbanded(From,To,Packet,[TO_ID,FDomain,NewTo]),
          ?ERROR_MSG("group user list is not found .. ~p",["not notify_group_disbanded"]);
        _ ->
          %% lifang update : JO and Json is unused
          %% The purpose is maybe to log
          %% case Packet of
          %%   {xmlel,<<"message">>,_Attrs,[{xmlel,<<"body">>,_Attrs2,[{xmlcdata,Json}]}]} -> Json;
          %%   _ -> Json = []
          %% end,
          %% {ok,JO,_} = rfc4627:decode(erlang:list_to_binary(Json)),
          case lists:member({type_util:to_binary(FDomain), type_util:to_binary(FromUser)},UserList) or (FromUser =:= <<"0">>) of
            false ->
              notify_out_group(From,To,Packet,[TO_ID,FDomain,NewTo, Groupname,GroupImage]);
            true -> 
              Roster = lists:map(fun({LDomain,LUser})-> 
                    LUID = type_util:to_binary(LUser),
                    LSERVER = type_util:to_binary(LDomain),
                    #jid{user=LUID,server=LSERVER,luser=LUID,lserver=LSERVER,resource=[],lresource=[]} 
              end,UserList),
              ?DEBUG("###### route_group_msg 002 :::> GroupId=~p ; Roster=~p",[GroupId,Roster]),
              ?DEBUG("group_message_title_~p src_msg_id=~p ; roster_size=~p",[GroupId,MID,length(Roster)]),
              lists:foreach(fun(Target)-> 
                #jid{user=TargetUser,server=TargetDomain}=Target,
                case (FromUser2 =:= TargetUser) and (FromDomain2 =:= TargetDomain) of
                  true ->
                    skip;
                  _->
                    route_msg(From, To,Target,Packet,GroupId,Groupname,Masklist,GroupImage,MID)
                end
              end,Roster),
              %%群组客服系统
              aa_mongodb:save_super_group_mongo(From, To, Packet, {GroupId,Groupname,GroupImage,UserList}),
              %aa_log:save_to_log({super_group,From,To,Packet,{GroupId,Groupname,GroupImage}}, Domain),
              %%群组活度
              aa_log:calculation_of_active_days(GroupId,Packet,Domain,GroupLevel,Members)
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
%% call by api
reload_group_user(Domain,GroupId) ->
  Response = get_user_list_by_group_id(do,Domain,GroupId),
  Group_cache_key = <<GroupId/binary,"@",Domain/binary,"/super_group_cache">>,
  case Response of 
    {ok,[],[],[],[],[],[]} ->
      skip;
    {not_found,[],[],[],[],[],[]} ->
      ?DEBUG("reload_group_user__clean__gid = ~p",[GroupId]),
      %% gen_server:call(aa_hookhandler,{ecache_cmd,["DEL",Group_cache_key]});
      %% 20141115: 防止在一个进程排队，产生瓶颈，特此修正
      aa_hookhandler:ecache_cmd(["DEL",Group_cache_key]);
    {ok,_,_,_,_,_,_} ->
      ?DEBUG("reload_group_user__set__gid = ~p",[GroupId]),
      %% gen_server:call(aa_hookhandler,{ecache_cmd,["SET",Group_cache_key,erlang:term_to_binary(Response)]});
      %% 20141115: 防止在一个进程排队，产生瓶颈，特此修正
      aa_hookhandler:ecache_cmd(["SET",Group_cache_key,erlang:term_to_binary(Response)]);
    _ ->
      skip
  end,
  Response.

%% =================================================
%% 发送单个用户群组消息
%% =================================================

route_msg(#jid{user=FromUser,server=_FromDomain}=From, #jid{server = _GroupDomain} = GTo, #jid{user=User,server=Domain}=To,Packet,GroupId,Groupname,Masklist,GroupImage,MID) ->
  {X,E,Attr,_Body} = Packet,
  ?DEBUG("##### route_group_msg_003 param :::> {User,Domain,GroupId,Masklist}=~p",[{User,Domain,GroupId,Masklist}]),
  DictAttr = dict:from_list(Attr),
  {M,S,SS} = now(),
  MsgTime = erlang:list_to_binary(lists:sublist(erlang:integer_to_list(M*1000000000000+S*1000000+SS),1,13)),
  MT = get_msgtype(DictAttr),
  TO_ID = erlang:integer_to_binary(M*1000000000000+S*1000000+SS),
  Mask = case lists:member({type_util:to_binary(Domain) , type_util:to_binary(User)},Masklist) of true -> <<"1">>; false-> <<"0">> end,
  % -------------------------------------------------
  % 重置id,to ,msgtype,msgtime等参数
  % 如果没有该key，则不做处理
  % -------------------------------------------------
  RAttr0 = lists:map(fun({K,V})-> 
        case K of 
          <<"id">> -> %%对隐藏消息做特殊处理
            if Mask =:= <<"1">> ->
                {<<"id">>,<<"hide_msg_",TO_ID/binary>>};
               true->
                {K,TO_ID}
            end;
          <<"to">> -> {K,<<User/binary,"@", Domain/binary>>};
          <<"msgtype">> -> {K,MT};  
          <<"msgTime">> -> skip;
          _-> {K,V} 
        end 
    end,Attr),

  RAttr1 = [{<<"mask">>,Mask},{<<"groupid">>,GroupId}|RAttr0],
  RAttr2 = lists:append([Kv||Kv<-RAttr1,Kv=/=skip],[{<<"msgTime">>,MsgTime}]),

  %% TODO Groupmember
  [JSON] = aa_log:get_text_message_from_packet(Packet), 
  StructJson = jiffy:decode(erlang:list_to_binary(JSON)),
  ?DEBUG("group struct json is ====> ~p",[StructJson]),
  Lang = "zh",

  %% -------------------------------------------------
  %% 修改J0和is_skip两个参数
  %% 原有注释:同一条消息不同的人显示的不一样
  %% -------------------------------------------------
  
  case MT of
    <<"system">> ->
      case ej:get({<<"type">>}, StructJson) of
        <<"102">>->
          InviteIdB = ej:get({<<"inviteid">>}, StructJson),
          %%  {ok,BeInviteId} = rfc4627:get_field(J, "beinviteid"),
          InviteName = ej:get({<<"invitename">>}, StructJson),
          BeInviteName = ej:get({<<"beinvitename">>}, StructJson),
          InviteId = binary_to_list(InviteIdB),
          IsSkip = false,
          InviteSourceB = ej:get({<<"invitesource">>}, StructJson, <<"skip">>),
          InviteId = type_util:to_list(InviteIdB),
          InviteSource = type_util:to_list(InviteSourceB),
          case (User =:= InviteId) and (InviteSource =:= Domain) of
            true ->
              Args = [binary_to_list(BeInviteName)],
              CountentKey = "system_102001";
            _->
              Args = [binary_to_list(InviteName),binary_to_list(BeInviteName)],
              CountentKey = "system_102002"
          end,
          NewType = <<"1000">>,
          Countentstr = list_to_binary(get_itdoc_str(Lang,CountentKey,Args)),
          Jx = ej:set({<<"content">>},StructJson,Countentstr),
          J0 = ej:set({"type"},Jx, NewType);
        
        <<"105">> ->
          IsSkip = false,
          InviteIdB = ej:get({<<"inviteid">>},StructJson),
          InviteSourceB = ej:get({<<"invitesource">>},StructJson),
          InviteId = type_util:to_list(InviteIdB),
          InviteSource = type_util:to_list(InviteSourceB),
          case (User =:= InviteId) andalso (InviteSource =:= Domain) of
            true->
              NewType = <<"105">>,
              %% Countentstr = <<"">>;
              Countentstr = list_to_binary(aa_super_group_chat:get_itdoc_str("zh","system_105002",[]));
            _->
              NewType = <<"1000">>,
              %% CountentKey = "system_105001",
              %% InviteName = ej:get({<<"invitename">>},StructJson),
              %%  Args = [type_util:to_list(InviteName)],
              %%  Countentstr = list_to_binary(get_itdoc_str(Lang,CountentKey,Args))
              ApplicantBin = ej:get({<<"invitename">>}, StructJson, <<"skip">>),
              Applicant = type_util:to_list(ApplicantBin),
              Countentstr = list_to_binary(aa_super_group_chat:get_itdoc_str("zh","system_105001",[Applicant]))
           end,
          Jx = ej:set({<<"content">>},StructJson, Countentstr),
          J0 = ej:set({<<"type">>}, Jx, NewType);
        
        %% {ok,<<"107">>}->    %%因为要有mask信息
        %%   {ok,ManagerList} = rfc4627:get_field(J, "managerlist"),
        %%   case lists:member(list_to_binary(User), ManagerList) of
        %%     true->
        %%       IsSkip = false,
        %%       {ok,Username} = rfc4627:get_field(J, "username"),
        %%       Args = [binary_to_list(Username)],
        %%       NewType = <<"1000">>,
        %%       Countentstr = list_to_binary(aa_super_group_chat:get_itdoc_str("zh","system_107",Args)),
        %%       Jx = rfc4627:set_field(J,"content",Countentstr),
        %%       J0 = rfc4627:set_field(Jx, "type", NewType);
        %%     _->
        %%       IsSkip = true,
        %%       J0 = J
        %%   end;
      
        <<"108">> ->    
          IsSkip = false,
          Bekickname = ej:get({<<"kickname">>},StructJson),
          Args = [binary_to_list(Bekickname)],
          NewType = <<"1000">>,
          CountentKey = "system_108002",
          Countentstr = list_to_binary(get_itdoc_str(Lang,CountentKey,Args)),
          Jx = ej:set({<<"content">>}, StructJson, Countentstr),
          J0 = ej:set({<<"type">>}, Jx, NewType);
        
        %%TODO
        <<"119">> ->
          IsSkip = false,
          AlterB = ej:get({<<"alter">>}, StructJson),
          GroupnameAlter = ej:get({<<"groupname">>}, StructJson),
          % Is_content_name = ej:get({<<"is_contain_name">>}, StructJson),
          Alter = erlang:binary_to_list(AlterB),
          if
            User =:= Alter ->     %%群资料通过通知要单独给修改人一个系统通知
              NewType = <<"116">>,
              Countentstr = <<"">>;
            true->
              NewType = <<"1000">>,       %%改资料
              CountentKey = "system_119",
              Args = [binary_to_list(GroupnameAlter)],
              Countentstr = list_to_binary(get_itdoc_str(Lang,CountentKey,Args))
          end,
          Jx = ej:set({<<"content">>}, StructJson, Countentstr),
          J0 = ej:set({<<"type">>}, Jx, NewType);
        
        <<"120">> ->
          AppointidB = ej:get({<<"appointid">>}, StructJson),
          Appointid = binary_to_list(AppointidB),
          if
            User =:= Appointid ->
              IsSkip = false,
              NewType = <<"1000">>,
              CountentKey = "system_120",
              Groupname = ej:get({<<"groupname">>}, StructJson),
              Args = [binary_to_list(Groupname)],
              Countentstr = list_to_binary(get_itdoc_str(Lang,CountentKey,Args)),
              Jx = ej:set({<<"content">>}, StructJson, Countentstr),
              J0 = ej:set({<<"type">>}, Jx, NewType);
            true->
              IsSkip = true,
              J0 = StructJson
          end;
        
        <<"121">> ->
          AppointidB = ej:get({<<"appointid">>}, StructJson),
          Appointid = binary_to_list(AppointidB),
          if
            User =:= Appointid ->
              IsSkip = false,
              NewType = <<"1000">>,
              CountentKey = "system_121",
              Groupname = ej:get({<<"groupname">>}, StructJson),
              Args = [binary_to_list(Groupname)],
              Countentstr = list_to_binary(get_itdoc_str(Lang,CountentKey,Args)),
              Jx = ej:set({<<"content">>}, StructJson, Countentstr),
              J0 = ej:set({<<"type">>}, Jx, NewType);
            true->
              IsSkip = true,
              J0 = StructJson
          end;

        undefined ->
          IsSkip = false,
          ?ERROR_MSG("missing body type!!!!MiD:~p",[MID]),
          J0 = StructJson;
        
        _Type->
          IsSkip = false,
          J0 = StructJson
      end;
    _->
      IsSkip = false,
      J0 = StructJson
  end,

  if
    IsSkip =:= false->
      J1 = ej:set({<<"groupname">>}, J0, Groupname),
      J2 = ej:set({<<"groupimage">>}, J1, GroupImage),
      J3 = ej:set({<<"mask">>}, J2, Mask),
      J4B = jiffy:encode(J3),
      ?DEBUG("json binary ===> ~p",[J4B]),
      RBody = [{xmlel,<<"body">>,[],[{xmlcdata,J4B}]}],
      RPacket = {X,E,RAttr2,RBody},
      %% 20141223 : 这里不能再用 otp ，瓶颈
      %% ?ERROR_MSG("3 .......................   ~p~n",[RPacket]),
      aa_hookhandler:handle_cast({group_chat_filter,GTo,To,RPacket,false},#state{}),
      ?DEBUG(" super_group to user ===>  ~p",[User]),
      case ejabberd_router:route(GTo, To, RPacket) of
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
    true->
      {ok,skip}
  end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_group_chat(#jid{server=Domain})->
  [GroupName|_] = str:tokens(Domain,<<".">>),
  case GroupName of 
    <<"group">> ->
        true;
    <<"super_group">> ->
        true;
    _ ->
        false
  end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_group_host_domain(Domain)->
  DomainTokens = str:tokens(Domain, <<".">>),
  case length(DomainTokens) > 2 of
    true->
      [_,D1,S2] = DomainTokens,
      <<D1/binary,".",S2/binary>>;
    _->
      Domain
  end.
      
set_super_group_msgsource(Domain, StructJson)->
  [D0|LD] = str:tokens(Domain, <<".">>),
  case D0 of
    <<"super_group">> ->
      [D1, D2] = LD,
      SourceKey = binary_to_list(<<"source_",D1/binary, ".",D2/binary>>), 
      Msgsource = list_to_binary(get_itdoc_str("zh",SourceKey,[])),
      ej:set({<<"msgsource">>},StructJson,Msgsource);
    _->
      StructJson
  end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%配置文件中的汉字取出来
get_itdoc_str(Lang,CountentKey,Args)->
  try
    case  ets:lookup(translations, {Lang, CountentKey}) of
        [{_, Trans}]->
        io_lib:format(Trans, Args);
      _->
        ""
    end
  catch
    _:_->
      ?ERROR_MSG("error:get_itdoc_str~p",[Lang,CountentKey,Args]),
      ""
  end.


%% =================================================
%% 获取群组成员信息和免打扰列表和群组头像、名称、级别
%% =================================================

get_group_member_info(Domain,UserListJO,Entity) ->
  UserList = lists:map(fun(UserJO)->
    if is_binary(UserJO) ->
        {Domain, UserJO};
      true ->
        UserDomain = ej:get({<<"source">>},UserJO, "skip"),
        UserId = ej:get({<<"uid">>},UserJO,"skip"),
        {UserDomain, UserId}
    end
  end, UserListJO),

 MasklistJO = ej:get({<<"masklist">>},Entity),
 MaskList = lists:map(fun(UserJO)->
   if is_binary(UserJO) ->
       {Domain, UserJO};
     true ->
       UserDomain = ej:get({<<"source">>},UserJO, Domain),
       UserId = ej:get({<<"uid">>},UserJO,"error"),
       {UserDomain, UserId}
   end
 end, MasklistJO),

 Groupname = ej:get({<<"groupname">>},Entity),
 
 GroupImage = ej:get({<<"groupimage">>},Entity),
 GroupLevel  = ej:get({<<"level">>},Entity),
 {ok,UserList,Groupname,MaskList,GroupImage,GroupLevel,erlang:length(UserList)}.


%% =================================================
%% 根据群组ID对应群组详细成员信息
%% =================================================

get_user_list_by_group_id(Domain,GroupId)->
  case GroupId of 
    "cctest" ->
      {ok,[<<"cc1">>,<<"cc2">>,<<"cc3">>],[],[],[],[],[]};
    _ ->
      case ejabberd_config:get_option({group_cache_enable,Domain},fun(V)->V end) of 
        true ->
          get_user_list_by_group_id(cache,Domain,GroupId);
        _ ->
          get_user_list_by_group_id(do,Domain,GroupId)
      end
  end.
get_user_list_by_group_id(cache,Domain,GroupId) ->
  Group_cache_key = <<GroupId/binary,"@",Domain/binary,"/super_group_cache">>,
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
  GroupId_bin = type_util:to_binary(GroupId),
  
  Method = <<"getUserList_super">>,
  Params = {[{<<"groupId">>,GroupId_bin}]},
  case aa_packet_filter:call_http(Domain,Method,Params) of
    {ok,Entity} ->
      case ej:get({<<"userlist">>},Entity) of
        undefined ->
          {not_found,[],[],[],[],[],[]};
        UserListJO ->
          get_group_member_info(Domain,UserListJO,Entity)
      end;
    {error,Error} ->
      ?ERROR_MSG("do get_user_list_by_group_id error .. ~p",[Error]),
      {error,Error};
    Exception ->
      ?ERROR_MSG("unknown exception .. ~p",[Exception]),
      {ok,[],[],[],[],[],[]}
  end.

%% ==========================================
%% 通知用户自己群组已经解散了，本条消息无法发送了
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
    %% lifang update json 
    %% {ok,J0,_} = rfc4627:decode("{}"),
    %% J1 = rfc4627:set_field(J0,"groupid",list_to_binary(GroupId)),
    %% J2 = rfc4627:set_field(J1,"groupname",<<"">>),
    %% J4 = rfc4627:set_field(J2,"type",<<"109">>),
    %% Json = rfc4627:encode(J4),
    J1 = {[{<<"groupid">>,list_to_binary(GroupId)},
           {<<"groupname">>,<<"">>},
           {<<"type">>,<<"109">>}
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
%% 通知用户已经被踢出了群组
%% =================================================

notify_out_group(#jid{user=FromUser,server=_FromDomain}=From,#jid{user=GroupId,server=GDomain}=To,Packet,[TO_ID,FDomain,NewTo, Groupname,GroupImage]) ->
    %%TODO 被T了
    %% <message id="xxxxx" from="1@yuejian.net" to"yy@yuejian.net" type="normal" msgtype=“system”>
    %%       <body>{groupid":"xx","groupname":"...","groupmember":[...],"type":"14"}</body>
    %% </message>
    {X,E,Attr,_} = Packet,
    RAttr = lists:map(fun({K,V})->
      case K of
        <<"id">>->                    {K,TO_ID};
        <<"from">> ->                 {K,<<GroupId/binary,"@",GDomain/binary>>};
        <<"to">> ->                   {K,<<FromUser/binary,"@",FDomain/binary>>};
        <<"type">> ->                 {K,<<"normal">>};
        <<"msgtype">> ->              {K,<<"system">>};
        _ ->                      {K,V} 
      end
    end,Attr),
    StructJson = {[{<<"groupid">>,list_to_binary(GroupId)},
             {<<"groupname">>,Groupname},
             {<<"type">>,<<"108">>},
             {<<"groupimage">>,GroupImage}
             ]},
    Json = jiffy:encode(StructJson),
    Body = [{xmlel,<<"body">>,[],[{xmlcdata,Json}]}],
    RPacket = {X,E,RAttr,Body},
    %% 20141223 : 这里不能再用 otp ，瓶颈
    % ?ERROR_MSG("2 .......................   ~p",[]),
    aa_hookhandler:handle_cast({group_chat_filter,From,NewTo,RPacket,false},#state{}),
    case ejabberd_router:route(To,From,RPacket) of
      ok ->
        ?DEBUG("route_group_type14 OK :::> {From,To,RPacket}=~p",[{To,From,RPacket}]);
        %% gen_server:cast(aa_hookhandler,{group_chat_filter,From,To,RPacket,false});
      Err ->
        ?DEBUG("route_group_type14 ERR=~p :::> {From,To,RPacket}=~p",[Err,{To,From,RPacket}]) 
    end.

%% =================================================
%% 从attrs属性里面提取msgtype字段
%% =================================================

get_msgtype(DictAttr) ->
  case dict:is_key(<<"msgtype">>,DictAttr) of 
    true-> 
      case dict:fetch(<<"msgtype">>,DictAttr) of
        <<"system">> ->
          <<"system">>;
        _ ->
          <<"super_groupchat">>
      end;
    _-> <<"super_groupchat">> 
  end.

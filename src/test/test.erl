-module(test).
-compile(export_all).
-include("include/ejabberd.hrl").
-include("include/logger.hrl").

-record(jid, {user, server, resource= <<"">>, luser, lserver, lresource= <<"">>}).
-record(xmlel, {name = <<"">>     :: binary(),
                attrs = []    :: [{string(), string()}],
                children = [] :: [{xmlcdata, iodata()} | xmlel()]}).

-type xmlel() :: #xmlel{}.
-record(xmlcdata, {
    cdata = <<>>
   }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 基础业务单元（封装的都是aa_hookhandler:user_send_packet_handler(From, To, Packet).）
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  

get_username_content_userimage(User) ->
  P = 
  [
   {<<"15475">>,{<<"李坊">>,<<"你好">>,<<"http://beta.iyueni.com/Uploads/avatar/2/15475_2VN7uq.jpg_200_200_2_80.jpg">>}},
   {<<"15209">>,{<<"领团">>,<<"你好">>,<<"http://beta.iyueni.com/Uploads/avatar/2/15209_O3j1yO.jpg_200_200_2_80.jpg">>}},
   {<<"15597">>,{<<"胡胡">>,<<"你好">>,<<"http://beta.yiyuread.com/Uploads/avatar/2/15174_5M0VqT.jpg_200_200_2_80.jpg">>}},
   {<<"15538">>,{<<"Jim">>,<<"你好">>,<<"http://beta.iyueni.com/Uploads/avatar/2/15538_pgL9AK.jpg_200_200_2_80.jpg">>}}
  ],
  proplists:get_value(User,P).
get_userid(N) ->
  N1 = random:uniform(N),
  lists:nth(N1,[<<"15475">>,<<"15209">>,<<"15538">>,<<"26">>,<<"15480">>,<<"13970">>]).
%% 所有参数
%% Fid      发送人
%% Tid      接收人
%% Domain   服务器
%% UserName 发送人名称、头像、性别
%% Gid      群组Id
%% Content  内容

-spec call_http(binary(), binary(), term()) -> {ok | error, any()} | term().
call_http(Domain,Method,Params)->
	{M,S,SS} = now(),
	SN = erlang:integer_to_list(M*1000000000000+S*1000000+SS),
 	HTTPTarget =  ejabberd_config:get_local_option({http_server,Domain},fun(V) -> V end),
	Params2 = {[ {<<"sn">> ,list_to_binary(SN)}, {<<"service">>,<<"ejabberd">>}, {<<"method">>,Method},{<<"params">>,Params}]}, 
	Json = jiffy:encode(Params2),
	Form = "body=" ++ binary_to_list(Json),
	case httpc:request(post,{ binary_to_list(HTTPTarget) ,[], "application/x-www-form-urlencoded" , Form },[],[] ) of
		{ok, {_,_,ResJson}} ->
	     case jiffy:decode(type_util:to_binary(ResJson)) of
	     	{error,Reason,ErrorCode} ->
           ?ERROR_MSG(" call_http error .. 1.sn = ~p, 2.reason = ~p, 3.errorcode = ~p",[SN,Reason,ErrorCode]),
					 {error,Reason};
	     	StructJson ->
	     		 StructJson
	     end;
		{error, Reason} ->
			?ERROR_MSG(" call_http error .. 1.sn=~p, 2.reason = ~p",[SN,Reason]),
			{error,Reason};
		Reason ->
			?ERROR_MSG(" call_http error .. 1.reason = ~p ",[Reason])
	end.

userinfo(Uid,Domain) ->
   Info = call_http(Domain,<<"get_user_info">>, {[{<<"uid">>,list_to_binary(Uid)}]}),
   Name = ej:get({<<"name">>},Info),
   Sex = ej:get({<<"sex">>},Info),
   Imageurl = ej:get({<<"imageurl">>},Info),
   {[{<<"userid">>,list_to_binary(Uid)},{<<"username">>,Name},{<<"usergender">>,Sex},{<<"userimage">>,Imageurl}]}.

route(Fid,Gid,Domain,Content) ->
  From = jlib:make_jid({Fid,Domain,<<>>}),
  To = jlib:make_jid({Gid,<<"super_group.",Domain/binary>>,<<>>}),
  Id = <<"07ed5f-3378-4271-ac9f-53bffc608ce3">>,
  MT = <<"super_groupchat">>,
  Type = <<"chat">>,
  J0 = userinfo(Fid,Domain),
  J1 = ej:set({<<"groupid">>},J0,list_to_binary(Gid)),
  J2 = ej:set({<<"type">>},J1,1000),
  J3 = ej:set({<<"content">>},J2,Content),
  J4 = ej:set({<<"msgsource">>},J3,<<231,186,166,228,189,160>>),
  J5 = ej:set({<<"usersource">>},J4,<<"yuejian.net">>),
  J6 = ej:set({<<"userfrom">>},J5,<<231,186,166,228,189,160>>),
  J7 = ej:set({<<"msgtype">>},J6,<<"0">>),
  Json = jiffy:encode(J7),
  Packet = #xmlel{name = <<"message">>,
                       attrs=[{<<"id">>,Id},{<<"to">>,To},{<<"msgtype">>,MT},{<<"type">>,Type}],
                       children=[#xmlel{name = <<"body">>,
                                             attrs=[],
                                             children=[{xmlcdata,Json}]}]},
  
  aa_hookhandler:user_send_packet_handler(From,To,Packet).

recursion(Domain,1=Content) ->
  Uid = get_userid(4),
  route(Uid,<<"1122592">>,Domain,Content);
recursion(Domain,Content) ->
  Uid = get_userid(4),
  route(Uid,<<"1122592">>,Domain,Content),
  recursion(Domain,Content-1).






%% =================================================
%% 15475 发送到群组 1122592 content
%% =================================================
one(Content) ->
  aa_hookhandler:user_send_packet_handler({jid,<<"15475">>,<<"yuejian.net">>,<<"app_v2.1.286344403386857">>,<<"15475">>,<<"yuejian.net">>,
       <<"app_v2.1.2863444033868577">>}, {jid,<<"1122592">>,<<"super_group.yuejian.net">>,<<>>,<<"1122592">>,
       <<"super_group.yuejian.net">>,<<>>}, {xmlel,<<"message">>,
              [{<<"id">>,<<"07ed5f-3378-4271-ac9f-53bffc608ce3">>},
               {<<"to">>,<<"1122592@super_group.yuejian.net">>},
               {<<"msgtype">>,<<"super_groupchat">>},
               {<<"type">>,<<"chat">>}],
              [{xmlel,<<"body">>,[],
                           [{xmlcdata,<<"{\"groupid\":\"1122592\",\"userid\":\"15475\",\"username\":\"leon\",  \"userimage\":\"http:\\/\\/beta.iyueni.com\\/Uploads\\/avatar\\/2\\/15475_2VN7uq.jpg_200_200_2_80.jpg\",\"usergender\":1,\"type\":1000,  \"content\":\"",Content/binary,"\",\"msgsource\":\"xx\",\"usersource\":\"yuejian.net\",\"userfrom\":\"xx\",\"msgtype\":\"0\"}">>}]}]}).

%% =================================================
%% User2 发送到 1122592群组 预定义content
%% =================================================
                       
one(user,User2) ->
  Timestamp = erlang:integer_to_binary(timestamp()),
  
  %% From = {jid,<<"15475">>,<<"yuejian.net">>,"app_v2.1.286344403386857",<<"15475">>,<<"yuejian.net">>,"app_v2.1.2863444033868577"},
  From = {jid,User2,<<"yuejian.net">>,"app_v2.1.286344403386857",User2,<<"yuejian.net">>,"app_v2.1.2863444033868577"},
  To = {jid,<<"1122592">>,<<"super_group.yuejian.net">>,<<>>,<<"1122592">>,<<"super_group.yuejian.net">>,<<>>},
  #jid{user=FUser,server=FServer,resource=_FResource}=From,
  #jid{user=TUser,server=TServer,resource=_TResource}=To,
  %% 用户相关信息
  Id = Timestamp,
  GroupId = TUser, 
  UserId = FUser,
  {UserName,Content,UserImage} = get_username_content_userimage(FUser),
  UserGender = 2,
  Type= 1000,
  MsgSource = <<"约你">>,
  UserSource = FServer,
  UserFrom = <<"约你">>,
  MsgType = <<"super_groupchat">>,
  
  Json = [{<<"groupid">>,GroupId},
          {<<"userid">>,UserId},{<<"username">>,UserName},{<<"userimage">>,UserImage},{<<"usergender">>,UserGender},{<<"type">>,Type},
          {<<"content">>,Content},
          {<<"msgsource">>,MsgSource},{<<"usersource">>,UserSource},{<<"userfrom">>,UserFrom},{<<"msgtype">>,0}],
  CData = jiffy:encode({Json}),
  Packet = #xmlel{name = <<"message">>,attrs=[{<<"id">>,Id},{<<"to">>,<<TUser/binary,"@",TServer/binary>>},{<<"msgtype">>,MsgType},{<<"type">>,<<"chat">>}],
                       children=[#xmlel{name = <<"body">>,attrs=[],
                                 children=[{xmlcdata,CData}]}]},
  aa_hookhandler:user_send_packet_handler(From,To,Packet);
  
one(1,Content) ->
  From = {jid,<<"15475">>,<<"yuejian.net">>,"app_v2.1.286344403386857",<<"15475">>,<<"yuejian.net">>,"app_v2.1.2863444033868577"},
  To = {jid,<<"1122592">>,<<"super_group.yuejian.net">>,<<>>,<<"1122592">>, <<"super_group.yuejian.net">>,<<>>},
  Packet = {xmlel,<<"message">>,
            [{<<"id">>,<<"07ed5f-3378-4271-ac9f-53bffc608ce3">>},
             {<<"to">>,<<"1122592@super_group.yuejian.net">>},
             {<<"msgtype">>,<<"super_groupchat">>},
             {<<"type">>,<<"chat">>}],
            [{xmlel,<<"body">>,[],
                         [{xmlcdata,<<"{\"groupid\":\"1122592\",\"userid\":\"15475\",\"username\":\"leon\",\"userimage\":\"http:\\/\\/beta.iyueni.com\\/Uploads\\/avatar\\/2\\/15475_2VN7uq.jpg_200_200_2_80.jpg\",\"usergender\":1,\"type\":1000,\"content\":\"",Content/binary,"\",\"msgsource\":\"xx\",\"usersource\":\"yuejian.net\",\"userfrom\":\"xx\",\"msgtype\":\"0\"}">>}]}]},
  aa_hookhandler:user_send_packet_handler(From, To, Packet).        

  

%% =================================================
%% 一个完整的单人发送
%% From To Content
%% =================================================
  
one(#jid{user=FUser,server=FServer,resource=FResource}=From,#jid{user=TUser,server=TServer,resource=TResource}=To,Content) ->
  
  UserId = FUser,
  %% UserId = <<"15475">>,
  UserName = <<"leon">>,
  UserImage = << "http:\\/\\/beta.iyueni.com\\/Uploads\\/avatar\\/2\\/15475_2VN7uq.jpg_200_200_2_80.jpg" >>,
  %% Content = << "北京领团科技__" >>,
  CData = <<"{\"groupid\":\"1122592\",\"userid\":\"", UserId/binary ,"\",\"username\":\"", UserName/binary, "\",\"userimage\":\"",  UserImage/binary,  "\",\"usergender\":1,\"type\":1000,\"content\":\"",Content/binary,"\",\"msgsource\":\"xx\",\"usersource\":\"yuejian.net\",\"userfrom\":\"xx\",\"msgtype\":\"0\"}">>,
  Data = #xmlcdata{cdata = CData},
  
  Packet = #xmlel{name = <<"message">>,
                       attrs=[{<<"id">>,<<"07ed5f-3378-4271-ac9f-53bffc608ce3">>},
                              {<<"to">>,<<"1122592@super_group.yuejian.net">>},
                              {<<"msgtype">>,<<"super_groupchat">>},
                              {<<"type">>,<<"chat">>}],
                       children=[
                               #xmlel{name = <<"body">>,
                                           attrs=[],
                                           children=[Data] } ]
                      },
  aa_hookhandler:user_send_packet_handler(From, To, Packet).
  
  
%% /4
one(From, To, UserName, Content) ->
  #jid{user=FUser,server=FServer,resource=FResource}=From,
  #jid{user=TUser,server=TServer,resource=TResource}=To,
  UserId = FUser,
  %% UserId = <<"15475">>,
  %% UserName = <<"leon">>,
  UserImage = << "http:\\/\\/beta.iyueni.com\\/Uploads\\/avatar\\/2\\/15475_2VN7uq.jpg_200_200_2_80.jpg" >>,
  %% Content = << "北京领团科技__" >>,
  CData = <<"{\"groupid\":\"1122592\",\"userid\":\"", UserId/binary ,"\",\"username\":\"", UserName/binary, "\",\"userimage\":\"",  UserImage/binary,  "\",\"usergender\":1,\"type\":1000,\"content\":\"",Content/binary,"\",\"msgsource\":\"xx\",\"usersource\":\"yuejian.net\",\"userfrom\":\"xx\",\"msgtype\":\"0\"}">>,
  Data = #xmlcdata{cdata = CData},
  
  Packet = #xmlel{name = <<"message">>,
                       attrs=[{<<"id">>,<<"07ed5f-3378-4271-ac9f-53bffc608ce3">>},
                              {<<"to">>,<<"1122592@super_group.yuejian.net">>},
                              {<<"msgtype">>,<<"super_groupchat">>},
                              {<<"type">>,<<"chat">>}],
                       children=[
                               #xmlel{name = <<"body">>,
                                           attrs=[],
                                           children=[Data] } ]
                      },
  %% timer:sleep(250),
  aa_hookhandler:user_send_packet_handler(From, To, Packet).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 扩展业务单元（封装的都是以one为基础的方法）
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  

send_jid() ->
  From = {jid,<<"15475">>,<<"yuejian.net">>,"app_v2.1.286344403386857",<<"15475">>,<<"yuejian.net">>,"app_v2.1.2863444033868577"}, 
  To = {jid,<<"1122592">>,<<"super_group.yuejian.net">>,<<>>,<<"1122592">>, <<"super_group.yuejian.net">>,<<>>},
  Content = << "北京领团科技__"/utf8 >>,
  one(From,To,Content).

%%   
send(name,Name,userid,UserId,content,Content) ->
  FUser = UserId,
  FServer = <<"yuejian.net">>,
  FResource = <<>>,
  From = #jid{user=FUser,server=FServer,resource=FResource,luser=FUser,lserver=FServer,lresource=FResource},
  To = {jid,<<"1122592">>,<<"super_group.yuejian.net">>,<<>>,<<"1122592">>, <<"super_group.yuejian.net">>,<<>>},
  one(From,To,Name,Content).
  

  

  
%% 模拟一个人无间隔发送多条  
send(N) ->
  lists:foldl(fun(Num,_Acc) ->
        NumBin = erlang:integer_to_binary(Num),
        Content = << "北京领团科技有限责任公司_"/utf8,  NumBin/binary >>,
        timer:sleep(25),
        one(Content)
      end,[],lists:seq(1,N)).

%% 模拟一个人间隔250ms发送多条      
send(Min,Max) ->
  lists:foldl(fun(Num,_Acc) ->
        NumBin = erlang:integer_to_binary(Num),
        Content = << "北京领团科技__"/utf8,  NumBin/binary >>,
        timer:sleep(250),
        one(Content)
      end,[],lists:seq(Min,Max)).


  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 复杂业务单元 （封装的都是以send为基础的业务单元）
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 一个人发送 N 条消息，标志第 Nth 批
send(1,NConstant,nth,Nth,name,Name,userid,UserBin) ->
  NameBin = erlang:list_to_binary(Name),
  UserId = erlang:binary_to_list(UserBin),
  NumBin = erlang:integer_to_binary(NConstant),
  %% NumBin = <<"1">>,
  NthBin = erlang:integer_to_binary(Nth),
  timer:sleep(1000),
  send(name,NameBin,userid,UserId,content,<< "第 ", NthBin/binary, " 批发送  ", NumBin/binary ," " >>);
send(N,NConstant,nth,Nth,name,Name,userid,UserBin) ->
  NameBin = erlang:list_to_binary(Name),
  UserId = erlang:binary_to_list(UserBin),
  NumBin = erlang:integer_to_binary(NConstant - N + 1),
  %% NumBin = <<"1">>,
  NthBin = erlang:integer_to_binary(Nth),
  timer:sleep(1000),
  send(name,NameBin,userid,UserId,content,<< "第 ", NthBin/binary, " 批发送  ", NumBin/binary ," " >>),
  send(N-1,NConstant,nth,Nth,name,Name,userid,UserBin).
  
%% 模拟多人发送


%% 发送 N 条 , Nth批
send_n(N,Nth) ->
  [ spawn(fun() -> {Random,_}=random:uniform_s(9, erlang:now()),timer:sleep(Random*100),send(N,N,nth,Nth,name,DomainBin,userid,UserBin) end )|| {DomainBin,UserBin} <- [{<<"张三">>,<<"15180">>},{<<"李四">>,<<"15186">>},{<<"王二">>,<<"15194">>},{<<"麻五">>,<<"15475">>}]].

send_mix(N) ->
  lists:foldl(fun(X,Acc) -> 
      timer:sleep(1333),  
       multi_one(X)
     end,[],lists:seq(1,N)).
     
send_mix(N,Nth) ->
  lists:foldl(fun(X,Acc) -> 
       send_n(N,X),
       %% 保证上一批发送完毕
       timer:sleep(N * 1000)
     end,[],lists:seq(1,Nth)).

%% 每人发送一条  content结尾为N     
multi_one(N) ->
  [ 
    begin
      NumBin = erlang:integer_to_binary(N),
      %% 每个用户间隔 330 ms发送一条消息
      F1 = fun() -> timer:sleep(0001), send(name,Name,userid,UserId,content,<< "第 "/utf8, NumBin/binary, " 批发送  1 "/utf8 >>) end,  
      F2 = fun() -> timer:sleep(0331), send(name,Name,userid,UserId,content,<< "第 "/utf8, NumBin/binary, " 批发送  2 "/utf8 >>) end,  
      F3 = fun() -> timer:sleep(0661), send(name,Name,userid,UserId,content,<< "第 "/utf8, NumBin/binary, " 批发送  3 "/utf8 >>) end,  
      F4 = fun() -> timer:sleep(0991), send(name,Name,userid,UserId,content,<< "第 "/utf8, NumBin/binary, " 批发送  4 "/utf8 >>) end,  
      F5 = fun() -> timer:sleep(1321), send(name,Name,userid,UserId,content,<< "第 "/utf8, NumBin/binary, " 批发送  5 "/utf8 >>) end,
      erlang:spawn(F1),      
      erlang:spawn(F2),      
      erlang:spawn(F3),      
      erlang:spawn(F4),      
      erlang:spawn(F5)   
    end
    || {Name,UserId} <- [{<<"张三"/utf8>>,<<"26">>},{<<"李四"/utf8>>,<<"13970">>},{<<"王二"/utf8>>,<<"15209">>},{<<"麻五"/utf8>>,<<"15475">>},{<<"风一"/utf8>>,<<"15480">>},{<<"当当"/utf8>>,<<"15538">>}] %%get_group_users(<<"yuejian.net">>,<<"1122592">>)
  ].
  
  
  


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 获取离线消息功能组 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
traverse_offline() ->
  {ok,Offline} = aa_hookhandler:ecache_cmd(["ZRANGE","15597@yuejian.net/offline_msg","0","-1"]),
  lists:foldl(fun(Offline_msgid,Acc)->
        OfflineMsg = get_one_offline(Offline_msgid),
        Acc ++ OfflineMsg
    end,[],Offline).
        
range_offline(OfflineMsgs) ->
  %% SortFun = fun({MsgTime,UserName,Content}, {MsgTime1,UserName2,Content3}) ->
  %%   UserName > UserName
  %% end,
  %% OfflineMsgs2 = lists:sort(SortFun, OfflineMsgs),
  OfflineMsgs2 = lists:keysort(2, OfflineMsgs),
  [io:format("~n~p  ~ts ~ts~n",[MsgTime,UserName,Content]) || {MsgTime,UserName,Content} <- OfflineMsgs2 ].

sorted_offline() ->
  range_offline(traverse_offline()).
  
get_one_offline(Offline_msgid) ->
  case aa_hookhandler:ecache_cmd( ["GET",Offline_msgid] ) of 
        {ok,Obj} when erlang:is_binary(Obj) ->
            {FF,TT,PP} = erlang:binary_to_term(Obj),
            %% Rtn = case ejabberd_router:route(FF, TT, PP) of
            %%       ok -> ok; 
            %%       Err -> "Error: "++Err
            %%   end;
            %% [{FF,TT,PP}];
            %% [{MsgTime,{ok,Content}}] = get_msgtime_content(PP),
            %% io:format("~n~nmsgtime = ~p~ncontent = ~ts~n~n",[MsgTime,Content]),
            [{MsgTime,UserName,Content}] = get_msgtime_username_content(PP),
            %% io:format("~nmsgtime = ~p  ~ts  ~ts~n",[MsgTime,UserName,Content]),
            %% io_lib:format("~s~ts~ts",[MsgTime,UserName,Content]);
            [{MsgTime,UserName,Content}];
            %% [];
            
        Other ->    
            %% CMD = ["ZREM",KEY,Offline_msgid],
            %% ZREM_R = aa_hookhandler:ecache_cmd(CMD)
            []
    end.
    
get_msgtime_content(Packet) ->
  case Packet of
    {xmlel,<<"message">>,Attr,_Children} ->
      MsgTime = fxml:get_tag_attr_s(<<"msgTime">>, Packet),
      OLdid = fxml:get_tag_attr_s(<<"id">>, Packet),
      [JSON] = aa_log:get_text_message_from_packet(Packet),
      StructJson = jiffy:decode(erlang:list_to_binary(JSON)),
      Content = ej:get({<<"content">>},StructJson),
      [{MsgTime,Content}];
    _ ->
      []
  end.
  
get_msgtime_username_content(Packet) ->
  case Packet of
    {xmlel,<<"message">>,Attr,_Children} ->
      MsgTime = fxml:get_tag_attr_s(<<"msgTime">>, Packet),
      OLdid = fxml:get_tag_attr_s(<<"id">>, Packet),
      [JSON] = aa_log:get_text_message_from_packet(Packet),
      StructJson = jiffy:decode(erlang:list_to_binary(JSON)),
      display(StructJson),
      {ok,Content} = ej:get({<<"content">>},StructJson),
      {ok,UserName} = ej:get({<<"username">>},StructJson),
      [{MsgTime,UserName,Content}];
    _ ->
      []
  end.
  

  
  
  
  
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% 业务工具集util  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_group_users(Domain,GroupId) ->
  {ok,Server_User,_,_,_,_,_} = aa_super_group_chat:get_user_list_by_group_id(Domain,GroupId),
  Server_User.
  
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% 语言工具集util  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   
%% timestamp() ->
%%   {M,S,_} = erlang:now(),
%%   M*1000000 + S.
timestamp() ->
 {M,S,SS} = now(),
 M*1000000000000+S*1000000+SS.
  
  
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% 测试实例example 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
test1() ->
%%  aa_super_group_chat:get_user_list_by_group_id(<<"yuejian.net">>,<<"1122592">>). 
%% {ok,[{<<"yuejian.net">>,<<"15180">>},
%%      {<<"yuejian.net">>,<<"15186">>},
%%      {<<"yuejian.net">>,<<"15194">>},
%%      {<<"yuejian.net">>,<<"15206">>},
%%      {<<"yuejian.net">>,<<"15207">>},
%%      {<<"yuejian.net">>,<<"15209">>},
%%      {<<"yuejian.net">>,<<"15210">>},
%%      {<<"yuejian.net">>,<<"15212">>},
%%      {<<"yuejian.net">>,<<"15215">>},
%%      {<<"yuejian.net">>,<<"15218">>},
%%      {<<"yuejian.net">>,<<"15220">>},
%%      {<<"yuejian.net">>,<<"15238">>},
%%      {<<"yuejian.net">>,<<"15239">>},
%%      {<<"yuejian.net">>,<<"15241">>},
%%      {<<"yuejian.net">>,<<"15475">>},
%%      {<<"yuejian.net">>,<<"15538">>}],
%%     <<231,190,164,231,187,132>>,
%%     [],
%%     <<"http://beta.iyueni.com/Uploads/group/icon/2/15475_zteE7W.jpg_200_200_2_70.jpg">>,
%%     <<"1">>,16}.
ok.

display(_Args) ->
  ok.


zh() ->
  [<<"约你"/utf8>>,<<"约你">>,"约你"].
zh2() ->
  J0 = ej:set({<<"aa">>},{[]},<<"约你"/utf8>>),
  Json = jiffy:encode(J0),
  [J0,Json].

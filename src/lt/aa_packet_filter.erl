-module(aa_packet_filter).

-export([do/1,reload/3,reload_all/2,get_cache_mask/2]).
-export([call_http/3]).
-compile(export_all).
-define(HTTP_HEAD,"application/x-www-form-urlencoded").
-include("include/ejabberd.hrl").
-include("include/jlib.hrl").
-include("include/logger.hrl").

%% 2014-10-19 : 应对 16 号的屏蔽需求
%% 需求一：
%% 是做关于普通单人聊天msgtype=“normalchat”的这种类型的消息提醒屏蔽功能，类似于当时做的拉黑功能，这个目前只做针对普通单人聊天消息的提示屏蔽功能，方案如下:
%% 针对单人聊天消息，web端会给你同步每个用户的消息提示屏蔽名单，类似于黑名单，我们发送普通msgtype=“normalchat”聊天消息时，你接收到消息以后需要拦截，判断接受者的屏蔽消息名单里是否包含发送者，如果包含则不做apns推送，并且给消息body里边增加mask字段 ，0 未屏蔽  1屏蔽   如下所示：
%% <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="chat" msgtype=“normalchat”>
%%      <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"0","content":"hello！"}
%% 	      </body>
%% 		       </message>
%% 			   这是你接收到的消息。
%% 
%% 			   <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="chat" msgtype=“normalchat”>
%% 			        <body>{"mask":"0","userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"0","content":"hello！"}
%% 					     </body>
%% 						      </message>
%% 							  这是你拦截以后转发出来的消息，增加了mask字段。
%% 
%% 							  我们客户端接收到消息以后根据mask字段来判断是否提示该消息。
							  
%% 需求二：
%% 是做关于普通单人聊天msgtype=“normalchat”的这种类型的消息关系字段显示，这个目前只做针对普通单人聊天消息的提示屏蔽功能，方案如下:
%% 
%% web接口提供一个获取两人的关系接口，XMPP每次发送聊天消息时到接口获取关系friend_log信息，然后附带在消息内发送出去，XMPP需要对关系进行缓存，关系是单向的，可以缓存10-20分钟来减少两个服务器之间的压力。
%% <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="chat" msgtype=“normalchat”>
%%      <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"0","content":"hello！"}
%% 	      </body>
%% 		       </message>
%% 			   这是你接收到的消息。
%% 
%% 
%% 			      <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="chat" msgtype=“normalchat”>
%% 				       <body>{"friend_log":"0","userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"0","content":"hello！"}
%% 					        </body>
%% 							   </message>
%% 							   这是你拦截以后转发出来的消息，其中friend_log字段是你蓝接到消息以后添加到消息里边的关系字段  0：陌生人；1：好友；2：二度好友
%% 
%% 							   我们客户端接收到消息以后根据friend_log字段来判断关系。

do({r141016,#jid{lserver=Domain,luser=FU},#jid{luser=TU,lserver=TDomain}=To,Packet})->
	[_,E|_] = tuple_to_list(Packet),
	OrigPacket = case E of 
		<<"message">> ->
			{X,E,Attr,RBody} = Packet,
			?DEBUG("Attr=~p", [Attr] ),
			D = dict:from_list(Attr),
			MT = case dict:is_key(<<"msgtype">>,D) of true-> dict:fetch(<<"msgtype">>,D); _-> <<"">> end,
			?DEBUG("ejabberd router filter .. ~n~p",[Packet]),
			[JSON] = aa_log:get_text_message_from_packet(Packet),	
			StructJson = jiffy:decode(erlang:list_to_binary(JSON)),
			?DEBUG("ejabberd router filter json .. ~n~p",[JSON]),
			Mid = fxml:get_attr_s(<<"id">>, Attr),
			FromBin = <<FU/binary,"@", Domain/binary>>, 
			ToBin = <<TU/binary, "@", TDomain/binary>>,
			case MT of 
				<<"normalchat">> ->
          JO_3 =
            try
							% -------------------------------------------------
							% 1.添加免打扰设置字段
							% -------------------------------------------------
              JO_1 = set_mask(Domain,FromBin,TDomain,ToBin,StructJson),	
              ?INFO_MSG("aa_packet_filter__JO_1==>~p",[JO_1]),
							% -------------------------------------------------
							% 2.添加好友关系设置字段
							% -------------------------------------------------
              JO_2 = set_friend_log(Domain,FromBin,TDomain,ToBin,JO_1),
							% -------------------------------------------------
							% 3.添加vip设置字段
							% -------------------------------------------------
              set_vip(Domain,FromBin,TDomain,ToBin,JO_2)
            catch
              E:R->
                ?ERROR_MSG("set info in message error : ~p",[{E,R, erlang:get_stacktrace()}]),
                StructJson
            end,

					J4B = jiffy:encode(JO_3),
					Body = [{xmlel,<<"body">>,[],[{xmlcdata,J4B}]}],
					%%隐藏消息特殊处理
					<<IsHide:9/binary,_RestId/binary>> = Mid,
					Attr1 =
						case IsHide of
							<<"hide_msg_">> ->
								Attr;
							_->
								case fxml:get_attr_s(<<"isoff">>, Attr) of
									<<"1">> ->
										Attr;
									_->
										case ej:get({<<"mask">>},JO_3) of
											% -------------------------------------------------
											% 免打扰的消息不保存离线消息，要删除掉redis存储的对应mid的消息
											% -------------------------------------------------
											<<"1">> ->
												KK = <<FU/binary,"@",Domain/binary,"/offline_msg">>,
												DelCmd = ["ZREM",KK,<<Mid/binary,"@",Domain/binary>>],
												aa_hookhandler:ecache_cmd(DelCmd),
												lists:keyreplace(<<"id">>, 1, Attr, {<<"id">>,<<"hide_msg_",Mid/binary>>});
											_ ->
												Attr
										end
								end	
						end,
					{X,E,Attr1,Body};
          
        <<"super_groupchat">> ->
          %% 当群组是个人的时候，添加上apns_show的一些字段
          case aa_group_chat:is_group_chat(To) of
            true -> 
              Packet;
            _ -> 
              JO_3 = 
                try
                  JO_1 = set_mask(TDomain,FromBin,TDomain,ToBin,StructJson)
                catch
                  E:R->
                    ?ERROR_MSG("set info in message error : ~p",[{E,R, erlang:get_stacktrace()}]),
                    StructJson
                end,
              
             J4B = jiffy:encode(JO_3),
             ?INFO_MSG("aa_packet_filter__Body==>~p",[J4B]),
             Body = [{xmlel,<<"body">>,[],[{xmlcdata,J4B}]}],
              {X,E,Attr,Body}
          end;
              
				<<"system">> ->
					TypeJR = ej:get({<<"type">>},StructJson),
					% -------------------------------------------------
					% 隐藏消息特殊处理
					% -------------------------------------------------
					<<IsHide:9/binary,_RestId/binary>> = Mid,
					Attr1 =
							case IsHide of
								<<"hide_msg_">> ->
									Attr;
								_->
									case  TypeJR of
										<<"20">>->
											KK = <<FU/binary, "@", Domain/binary, "/offline_msg">>,
											DelCmd = ["ZREM",KK,<<Mid/binary, "@", Domain/binary>>],
											aa_hookhandler:ecache_cmd(DelCmd),
											lists:keyreplace(<<"id">>, 1, Attr, {<<"id">>, <<"hide_msg_", Mid/binary>>});
										<<"19">> ->
											KK = <<FU/binary, "@", Domain/binary, "/offline_msg">>,
											DelCmd = ["ZREM",KK,<<Mid/binary, "@", Domain/binary>>],
											aa_hookhandler:ecache_cmd(DelCmd),
											lists:keyreplace(<<"id">>, 1, Attr, {<<"id">>, <<"hide_msg_", Mid/binary>>});
										_->
											case ej:get({<<"mask">>},StructJson) of
												<<"1">> ->
													KK = <<FU/binary, "@", Domain/binary, "/offline_msg">>,
													DelCmd = ["ZREM",KK,<<Mid/binary, "@", Domain/binary>>],
													aa_hookhandler:ecache_cmd(DelCmd),
													lists:keyreplace(<<"id">>, 1, Attr, {<<"id">>, <<"hide_msg_", Mid/binary>>});
												_->
													Attr
											end
									end
							end,
					% -------------------------------------------------
					% 推送消息特殊处理
					% -------------------------------------------------
					if
						TypeJR =:= <<"100">>;TypeJR =:= <<"120">>;TypeJR =:= <<"121">>;
						TypeJR =:= <<"107">>;TypeJR =:= <<"108">>;TypeJR =:= <<"109">>;
						TypeJR =:= <<"110">>;TypeJR =:= <<"111">>;TypeJR =:= <<"1000">> ->
						J1 = ej:set({<<"push">>},StructJson, <<"0">>),
						J4B = jiffy:encode(J1),
						Body = [{xmlel,<<"body">>,[],[{xmlcdata,J4B}]}],
						{X,E,Attr1,Body};
					true->
						{X,E,Attr1,RBody}
					end;
				_ ->
					%特殊处理 下个版本要删掉
					case ej:get({<<"at">>},StructJson) of
						undefined ->
						  Packet;
					  CheckAt ->
							case is_binary(CheckAt) of
								true->
									J0 = ej:delete({<<"at">>},StructJson),
									J4B = jiffy:encode(J0),
									Body = [{xmlel,<<"body">>,[],[{xmlcdata,J4B}]}],
									{X,E,Attr,Body};
								false->
									Packet
							end

					end
			end;
		_ ->
			Packet	
	end,
	?INFO_MSG("aa_packet_filter__result==>~n  ~p",[OrigPacket]),
	OrigPacket.

get_jid(JIDStr) ->
	[U,DR] = string:tokens(JIDStr,"@"),
	[D|_] = string:tokens(DR,"/"),
	U++"@"++D.

set_mask(_Domain,FromBin,TDomain,ToBin,StructJson) ->
			%% 先到缓存里找，如果没有则回调并初始化缓存;
      [FromStr,ToStr]	= [get_jid(type_util:to_list(FromBin)),get_jid(type_util:to_list(ToBin))],
			Key = "mask__"++FromStr++ToStr,
			
			%% case gen_server:call(aa_hookhandler,{ecache_cmd,["GET",Key]}) of
			%% 20141115 : 这里有瓶颈，不能排队取,特此修改
			case aa_hookhandler:ecache_cmd(["GET",Key]) of
				{ok,Bin} when erlang:is_binary(Bin) ->
					?INFO_MSG("aa_packet_filter__set_mask__on_cache key=~p ; mask=~p",[Key,Bin]),
					%% 150110: get_mask_user接口有些变动,增加一个push参数，如果这个参数为1就推送否则不推	
					%% 需求来自 戚银
          MaskPush = type_util:to_list(Bin),
					[Mask,Push, MaskAll, ApnsShow] = 
          case string:tokens(MaskPush,",") of 
						[A1, A2, A3, A4] ->
							[A1, A2, A3, A4];
						_ ->
              aa_hookhandler:ecache_cmd(["DEL",Key]),
							["0","1", "0", "1"]
          end,
          J1 = ej:set({<<"mask">>},StructJson,type_util:to_binary(Mask)),
          J2 = ej:set({<<"push">>},J1,type_util:to_binary(Push)),
          J3 = ej:set({<<"mask_all">>},J2,type_util:to_binary(MaskAll)),
          ej:set({<<"apns_show">>},J3,type_util:to_binary(ApnsShow));
        _ ->
					Params = {[{<<"from">>,FromBin},{<<"to">>,ToBin}]},
					case call_http(TDomain,<<"get_mask_user">>,Params) of 
						{ok,Entity} ->	
							MaskBin = ej:get({<<"mask">>}, Entity,<<"0">>),
							Mask =  type_util:to_list(MaskBin),
              % -------------------------------------------------
							% 150110: get_mask_user接口有些变动,增加一个push参数，如果这个参数为1就推送否则不推	
              % 需求来自 戚银
							% -------------------------------------------------
							PushBin = ej:get({<<"push">>},Entity,"0"),
              Push = type_util:to_list(PushBin),

              ApnsShowBin = ej:get({<<"apns_show">>},Entity, "1"),
              ApnsShow = type_util:to_list(ApnsShowBin),

              MaskAllBin = ej:get({<<"mask_all">>},Entity, "0"),
							MaskAll =  type_util:to_list(MaskAllBin),
							%% gen_server:call(aa_hookhandler,{ecache_cmd,["SET",Key,Mask]}),
							%% 20141115 : 这里有瓶颈，不能排队取,特此修改
							Val = Mask++","++Push ++ ","++ MaskAll ++ "," ++ ApnsShow,
							aa_hookhandler:ecache_cmd(["SET",Key,Val]),
							Key_idx = "mask_set__"++ToStr,
							{M1,S1,T1} = now(), 
							Scope = integer_to_list(M1*1000000000000+S1*1000000+T1),
							%% gen_server:call(aa_hookhandler,{ecache_cmd,["ZADD",Key_idx,Scope,Key]}),
							%% 20141115 : 这里有瓶颈，不能排队取,特此修改
							aa_hookhandler:ecache_cmd(["ZADD",Key_idx,Scope,Key]),
              NewMask =
              if 
                Mask =:= "0" ->
                  if
                    MaskAll =:= "0" ->
                      "0";
                    true ->
                      {_,{NowHour,_,_}} = calendar:local_time(),
                      case (NowHour >= 22) and (NowHour =< 7) of
                        true->
                          "1";
                        _->
                          "0"
                      end
                  end;
                true ->
                  "1"
              end,
              J1 = ej:set({<<"mask">>},StructJson,type_util:to_binary(NewMask)),
              J2 = ej:set({<<"push">>},J1,type_util:to_binary(Push)),
              J3 = ej:set({<<"mask_all">>},J2,type_util:to_binary(MaskAll)),
              ej:set({<<"apns_show">>},J3,type_util:to_binary(ApnsShow));
						_ ->
							StructJson
					end
			end.

get_cache_mask(From,To)->
	Key = binary_to_list(<<"mask__",From/binary,To/binary>>),
	case aa_hookhandler:ecache_cmd(["GET",Key]) of
				{ok,Bin} when erlang:is_binary(Bin) ->
					?INFO_MSG("aa_packet_filter__set_mask__on_cache key=~p ; mask=~p",[Key,Bin]),
					%% 150110: get_mask_user接口有些变动,增加一个push参数，如果这个参数为1就推送否则不推	
					%% 需求来自 戚银
          MaskPush = type_util:to_list(Bin),
					case string:tokens(MaskPush,",") of 
            [A1, A2, A3, A4] ->
							[A1, A2, A3, A4];
						_ ->
              aa_hookhandler:ecache_cmd(["DEL",Key]),
							["0","1", "0", "1"]
          end;
			_->
        ["0","1","0","1"]
	end.

set_friend_log(Domain,FromBin,TDomain,ToBin,StructJson) ->
  case aa_config:enable_friend(Domain, TDomain) of
    true ->
      case ej:get({<<"friend_log">>},StructJson) of 
				% -------------------------------------------------
				% 缓存中没有friendlog字段
				% -------------------------------------------------
        undefined ->
          %% 2014-10-30 : 先到缓存里找，如果没有则回调并初始化缓存;
          [FromStr,ToStr]	= [get_jid(type_util:to_list(FromBin)),get_jid(type_util:to_list(ToBin))],
          Key = "friend_log__"++FromStr++ToStr,
          %% case gen_server:call(aa_hookhandler,{ecache_cmd,["GET",Key]}) of
          %% 20141115 : 这里有瓶颈，不能排队取,特此修改
          case aa_hookhandler:ecache_cmd(["GET",Key]) of
            {ok,Bin} when erlang:is_binary(Bin) ->
              ej:set({<<"friend_log">>},StructJson,Bin);
            _ ->
							Params = {[{<<"from">>,FromBin},{<<"to">>,ToBin}]},
              case call_http(TDomain,<<"get_relation">>,Params) of 
                {ok,Entity} ->	
                  Friend_log = ej:get({<<"friend_log">>},Entity),
                  Is_member = ej:get({<<"is_member">>}, Entity),
                  Member_type = ej:get({<<"member_type">>},Entity),
                  Member_level = ej:get({<<"member_level">>},Entity),
                  %% 5 分钟过期
                  aa_hookhandler:ecache_cmd(["PSETEX",Key,integer_to_list(1000*60*5),Friend_log]),
                  J1 = ej:set({<<"friend_log">>},StructJson,Friend_log),
                  VipInfo = type_util:to_list(Is_member) ++ ":"++ type_util:to_list(Member_type) ++ ":" ++ type_util:to_list(Member_level),
                  VipKey = "vip_key_" ++ FromStr,
                  aa_hookhandler:ecache_cmd(["SETEX",VipKey,integer_to_list(60*60*24),VipInfo]),
                  J2 = ej:set({<<"vip_level">>},J1,Member_level),
                  J3 = ej:set({<<"vip_type">>},J2,Member_level),
                  ej:set({<<"is_vip">>},J3,Is_member);
                _ ->
                  StructJson
              end
          end;
				_FriendLog ->
          StructJson
       end;
     _->
       StructJson
    end.	

set_vip(Domain,FromBin,TDomain,ToBin,StructJson) ->
  case aa_config:enable_friend(Domain, TDomain) of
    true->
      case ej:get({<<"is_vip">>},StructJson) of 
        undefined ->
          %% 2014-10-30 : 先到缓存里找，如果没有则回调并初始化缓存;
          FromStr	= get_jid(type_util:to_list(FromBin)),
          Key = "vip_key_"++FromStr,
          %% case gen_server:call(aa_hookhandler,{ecache_cmd,["GET",Key]}) of
          %% 20141115 : 这里有瓶颈，不能排队取,特此修改
          case aa_hookhandler:ecache_cmd(["GET",Key]) of
            {ok,Bin} when erlang:is_binary(Bin) ->
              ?INFO_MSG("aa_packet_filter__set_friend_log__on_cache key=~p ; mask=~p",[Key,Bin]),
              [Is_member,Member_type ,Member_level] = string:tokens(type_util:to_list(Bin),":"),
              J1 = ej:set({<<"vip_level">>},StructJson,list_to_binary(Member_level)),
              J2 = ej:set({<<"vip_type">>},J1,list_to_binary(Member_type)),
              ej:set({<<"is_vip">>},J2,list_to_binary(Is_member));
            _ ->
							Params = {[{<<"from">>,FromBin},{<<"to">>,ToBin}]},
              case call_http(TDomain,<<"get_relation">>,Params) of 
                {ok,Entity} ->	
                  Friend_log = ej:get({<<"friend_log">>},Entity),
                  Is_member = ej:get({<<"is_member">>},Entity),
                  Member_type = ej:get({<<"member_type">>},Entity),
                  Member_level = ej:get({<<"member_level">>},Entity),
                  ?INFO_MSG("aa_packet_filter__set_friend_log__on_http key=~p ; mask=~p",[Key,Friend_log]),
                  %% 5 分钟过期
                  aa_hookhandler:ecache_cmd(["PSETEX",Key,integer_to_list(1000*60*5),Friend_log]),
                  J1 = ej:set({<<"friend_log">>},StructJson,Friend_log),
                  VipInfo = type_util:to_list(Is_member) ++ ":"++ type_util:to_list(Member_type) ++ ":" ++ type_util:to_list(Member_level),
                  VipKey = "vip_key_" ++ FromStr,
                  aa_hookhandler:ecache_cmd(["SETEX",VipKey,integer_to_list(60*60*24),VipInfo]),
                  J2 = ej:set({<<"vip_level">>},J1,Member_level),
                  J3 = ej:set({<<"vip_type">>},J2,Member_type),
                  ej:set({<<"is_vip">>},J3,Is_member);
                _ ->
                  StructJson
              end
          end;
				_IsVip ->
          StructJson
      end;
    _->
      StructJson
  end.	

call_http(Domain,Method,Params)->
	{M,S,SS} = now(),
	SN = erlang:integer_to_list(M*1000000000000+S*1000000+SS),
 	HTTPTarget =  ejabberd_config:get_option({http_server,type_util:to_binary(Domain)},fun(V)->V end),
	Params2 = {[ {<<"sn">> ,list_to_binary(SN)}, {<<"service">>,<<"ejabberd">>}, {<<"method">>,Method},{<<"params">>,Params}]}, 
	Json = jiffy:encode(Params2),
	Form = "body=" ++ binary_to_list(Json),
	case httpc:request(post,{ binary_to_list(HTTPTarget) ,[], ?HTTP_HEAD , Form },[],[] ) of
		{ok, {_,_,ResJson}} ->
	     case jiffy:decode(type_util:to_binary(ResJson)) of
	     	{error,Reason,ErrorCode} ->
           ?ERROR_MSG(" call_http error .. 1.sn = ~p, 2.reason = ~p, 3.errorcode = ~p",[SN,Reason,ErrorCode]),
					 {error,Reason};
				{[{<<"errcode">>,ErrorCode},{<<"msg">>,  Reason}]} ->
					 ?ERROR_MSG(" call_http error .. 1.sn = ~p, 2.reason = ~p, 3.errorcode = ~p",[SN,Reason,ErrorCode]),
           {error,Reason}; 
	     	StructJson ->
	     		 get_entity(StructJson)
	     end;
		{error, Reason} ->
			?ERROR_MSG(" call_http error .. 1.sn=~p, 2.reason = ~p",[SN,Reason]),
			{error,Reason};
		Reason ->
			?ERROR_MSG(" call_http error .. 1.reason = ~p ",[Reason])
	end.

get_entity(StructJson) ->
	case ej:get({<<"success">>},StructJson) of
		true ->
			case ej:get({<<"entity">>},StructJson) of
				undefined ->
					?ERROR_MSG("call http error .. no entity ~n\t~p",[StructJson]),
					{error,no_entity};
				Entity ->
					{ok,Entity}
			end;
		_ ->
      {error,not_success}
	end.

reload(mask,FromStr,ToStr) ->
	[From,To] = [get_jid(FromStr),get_jid(ToStr)],		
	Key = "mask__"++From++To,
	%% gen_server:call(aa_hookhandler,{ecache_cmd,["DEL",Key]}),
	%% 20141115 : 这里有瓶颈，不能排队取,特此修改
	aa_hookhandler:ecache_cmd(["DEL",Key]),
	?INFO_MSG("reload__mask__key=~p",[Key]),
	ok;
reload(friend_log,FromStr,ToStr) ->
	[From,To] = [get_jid(FromStr),get_jid(ToStr)],		
	Key = "friend_log__"++From++To,
	Key1 = "friend_log__"++To++From,
	%% gen_server:call(aa_hookhandler,{ecache_cmd,["DEL",Key]}),
	%% 20141115 : 这里有瓶颈，不能排队取,特此修改
	aa_hookhandler:ecache_cmd(["DEL",Key]),
	aa_hookhandler:ecache_cmd(["DEL",Key1]),
	?INFO_MSG("reload__friend_log__key=~p",[Key]),
	ok.

reload_all(mask,ToStr) ->
	To = get_jid(ToStr),
	Key_idx = "mask_set__"++To,
	CMD = ["ZRANGE",Key_idx,"0","-1"],
	?INFO_MSG("reload_all_mask_cmd=~p",[CMD]),
	%% case gen_server:call(aa_hookhandler,{ecache_cmd,CMD}) of 
	%% 20141115 : 这里有瓶颈，不能排队取,特此修改
	case aa_hookhandler:ecache_cmd(CMD) of 
		{ok,Idxs} ->
			%% gen_server:call(aa_hookhandler,{ecache_cmd,["DEL",Key_idx]}),
			%% 20141115 : 这里有瓶颈，不能排队取,特此修改
			aa_hookhandler:ecache_cmd(["DEL",Key_idx]),
			?INFO_MSG("reload_all_mask_cmd=~p",[["DEL",Key_idx]]),
			lists:foreach(fun(KeyBin)->
        Key = type_util:to_list(KeyBin),
				?INFO_MSG("reload_all_mask_item_cmd=~p",[["DEL",Key]]),
				%% gen_server:call(aa_hookhandler,{ecache_cmd,["DEL",Key]}) 
				%% 20141115 : 这里有瓶颈，不能排队取,特此修改
				aa_hookhandler:ecache_cmd(["DEL",Key]) 
			end,Idxs);
		_ ->
			skip
	end.		



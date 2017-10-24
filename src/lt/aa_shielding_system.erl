%% @author Administrator
%% @doc @todo Add description to aa_shielding_system.


-module(aa_shielding_system).

-include("include/ejabberd.hrl").
-include("include/jlib.hrl").
-include("include/logger.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/0,add_blocked/3,remove_blocked/2,check_is_blocked/3]).

init()->
	[].

add_blocked(BLOCKEDID_LIST,TIME_LIST,TypeLists)->
	BlockedList = lists:zip3(BLOCKEDID_LIST, TIME_LIST,TypeLists),
	?WARNING_MSG("aa_shielding_system:add_blocked~p",[{BLOCKEDID_LIST,TIME_LIST,TypeLists}]),
	lists:foreach(fun({Blocked,Time,Typezip})->
						  lists:foreach(fun(Type)->
													BlockedKey = 
													  if
														  Type  =:= <<"all">>->
													 		 "be_blocked_user_"++binary_to_list(Blocked);
														  Type  =:= <<"normalchat">>->
															  "be_blocked_user_normalchat_"++binary_to_list(Blocked);
														  Type =:= <<"groupchat">>->
															  "be_blocked_user_groupchat_"++binary_to_list(Blocked);
														  Type =:= <<"super_groupchat">>->
															  "be_blocked_user_super_groupchat_"++binary_to_list(Blocked);
														  true->
															  skip
													  end,
												  if
													  Type =/= skip->
														  if
															  Time =:= 0->									%%谨记 0的时候是永久禁言
																  Cmd = ["SET",BlockedKey,true];
															  true->
																  Cmd = ["SETEX",BlockedKey,integer_to_list(binary_to_integer(Time)*60*60),true]
														  end,
														  aa_hookhandler:ecache_cmd(Cmd);
													  true->
														  skip
												  end		
												end, Typezip)
				  end, BlockedList),
	ok.

remove_blocked(BLOCKEDID_LIST,TypeList)->
	?WARNING_MSG("aa_shielding_system:remove_blocked~p",[{BLOCKEDID_LIST}]),
	BlockZip = lists:zip(BLOCKEDID_LIST, TypeList),
	lists:foreach(fun({BLOCKEDID,TypeL})->
						  lists:foreach(fun(Type)->
													if
														 Type =:= <<"all">>->	%%如果是全解禁，就把所有的都删了
															  BlockedKey = "be_blocked_user_"++binary_to_list(BLOCKEDID),
															 Cmd = ["DEL",BlockedKey],
															 aa_hookhandler:ecache_cmd(Cmd),
															  BlockedKey1 = "be_blocked_user_normalchat_"++binary_to_list(BLOCKEDID),
															 Cmd1 = ["DEL",BlockedKey1],
															 aa_hookhandler:ecache_cmd(Cmd1),
															  BlockedKey2 = "be_blocked_user_groupchat_"++binary_to_list(BLOCKEDID),
															 Cmd2 = ["DEL",BlockedKey2],
															 aa_hookhandler:ecache_cmd(Cmd2),
															  BlockedKey3 = "be_blocked_user_super_groupchat_"++binary_to_list(BLOCKEDID),
															 Cmd3 = ["DEL",BlockedKey3],
															 aa_hookhandler:ecache_cmd(Cmd3);
														 Type =:= <<"normalchat">>->
															  BlockedKey1 = "be_blocked_user_normalchat_"++binary_to_list(BLOCKEDID),
															 Cmd1 = ["DEL",BlockedKey1],
															 aa_hookhandler:ecache_cmd(Cmd1);
														 Type =:= <<"groupchat">>->
															 BlockedKey2 = "be_blocked_user_groupchat_"++binary_to_list(BLOCKEDID),
															 Cmd2 = ["DEL",BlockedKey2],
															 aa_hookhandler:ecache_cmd(Cmd2);
														 Type =:= <<"super_groupchat">>->
															 BlockedKey3 = "be_blocked_user_super_groupchat_"++binary_to_list(BLOCKEDID),
															 Cmd3 = ["DEL",BlockedKey3],
															 aa_hookhandler:ecache_cmd(Cmd3);
														 true->
															 skip
													 end
												end, TypeL)
						  end, BlockZip),
	ok.
%%检测该句聊天是否是被屏蔽的人发出来的
check_is_blocked(FJid,TJid,Packet)->
	#jid{luser = TUser, lserver = TDomain} = TJid,
	#jid{luser = FUser, lserver = FDomain} = FJid,
    %TODO
	MsgType = fxml:get_tag_attr_s(<<"msgtype">>, Packet),
	case (MsgType =:= <<"msgStatus">>) or(FUser =:= <<"stranger_limit">>) of
		true->
			false;
		_->
		case  (aa_hookhandler:is_little_secretary(TUser)) or aa_group_chat:is_group_chat(TJid) of
			true->
				false;
			false->
				#jid{luser = FUser} = FJid,
				FBLOCKEDID = binary_to_list(FUser),
				case aa_hookhandler:ecache_cmd(["GET","be_blocked_user_"++FBLOCKEDID]) of
					{ok,<<"true">>} ->
						?WARNING_MSG("check_is_~p:~p",["be_blocked_user_"++FBLOCKEDID,FBLOCKEDID]),
						true;
					_->
						if
							MsgType =:= <<"normalchat">>->
								case aa_hookhandler:ecache_cmd(["GET","be_blocked_user_normalchat_"++FBLOCKEDID]) of
									{ok,<<"true">>} ->
										?WARNING_MSG("check_is_~p:~p",["be_blocked_user_normalchat_"++FBLOCKEDID,FBLOCKEDID]),
										true;
									_->
										false
								end;
							MsgType =:= <<"groupchat">>->
								case aa_hookhandler:ecache_cmd(["GET","be_blocked_user_groupchat_"++FBLOCKEDID]) of
									{ok,<<"true">>} ->
										?WARNING_MSG("check_is_~p:~p",["be_blocked_user_groupchat_"++FBLOCKEDID,FBLOCKEDID]),
										true;
									_->
										false
								end;
							MsgType =:= <<"super_groupchat">> ->
								case aa_hookhandler:ecache_cmd(["GET","be_blocked_user_super_groupchat_"++FBLOCKEDID]) of
									{ok,<<"true">>} ->
										?WARNING_MSG("check_is_~p:~p",["be_blocked_user_super_groupchat_"++FBLOCKEDID,FBLOCKEDID]),
										true;
									_->
										false
								end;
							true->
								false
						end
				end
		end
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================



%% @author Administrator
%% @doc @todo Add description to lt_ios_provider_manager.

-module(aa_ios_provider_send).

-include("include/ejabberd.hrl").


%% -record(apns_push,
%% {
%%   	msgid,					%%消息id
%% 	from,					%%来源jid
%% 	to,						%%目标jid
%% 	msgtype,				%%消息类型
%% 	msgbody,				%%消息体
%% 	num						%%数量
%% }).

-define(APNS_POOL,apns_pool).
-define(HTTPHead,"application/x-www-form-urlencoded").
%% ====================================================================
%% API functions
%% ====================================================================
-export([send/2]).


send(Node,Message)->
%% 	ApnsMessage = 
%% 	try
%% 		deel_apns_msg(Message)
%% 	catch
%% 		E:R->
%% 			?ERROR_MSG("!!!!!deel_apns_msg error ~p R:~p",[Message,{E,R,erlang:get_stacktrace()}]),
%% 			skip	
%% 	end,
%% 	?WARNING_MSG("apns~p",[{Node,Message,ApnsMessage}]),
%% 	#apns_push{to = To,num = Num} = Message,
%% 	[Toid|_] = string:tokens(To,"@"),
%% 	HTTPTarget =  ejabberd_config:get_local_option({http_server,Toserver}),
	rpc:cast(Node, apns_sender, send, [Message]).

%% ====================================================================
%% Internal functions
%% ====================================================================

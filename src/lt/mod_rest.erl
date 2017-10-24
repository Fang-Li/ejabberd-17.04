-module(mod_rest).
-author('sneakin@semanticgap.com').

-behavior(gen_mod).

-export([start/2,
	 stop/1,
	 process/2
	]).

-compile(export_all).
-include("include/ejabberd.hrl").
-include("include/jlib.hrl").
-include("include/ejabberd_http.hrl").
-include("include/logger.hrl").

-record(session, {sid, usr, us, priority, info}).

start(_Host, _Opts) ->
	?DEBUG("Starting: ~p ~p", [_Host, _Opts]),
    	RESTSupervisor = { ejabberd_mod_rest_sup, 
		{ ejabberd_tmp_sup, start_link, [ejabberd_mod_rest_sup, ejabberd_mod_rest] },
    		permanent,
    		infinity,
    		supervisor,
    		[ejabberd_tmp_sup]
    	},
    	case supervisor:start_child(ejabberd_sup, RESTSupervisor) of
		{ok, _Pid} -> ok;
		{ok, _Pid, _Info} -> ok;
		{error, Error } -> {'EXIT', {start_child_error, Error}}
    	end.

stop(_Host) ->
	case supervisor:terminate_child(ejabberd_sup, ejabberd_mod_rest_sup) of
		ok -> ok;
		{error, Error} -> {'EXIT', {terminate_child_error, Error}}
	end.

process([], #request{method = 'POST', data = Data, ip = Ip }) ->
	?INFO_MSG("####rest ::> data=~p;",[Data]), 
	Stanza = xml_stream:parse_element(Data),
	From = jlib:string_to_jid(fxml:get_tag_attr_s(<<"from">>, Stanza)),
	To = jlib:string_to_jid(fxml:get_tag_attr_s(<<"to">>, Stanza)),
	Log = [jlib:jid_to_string(From), Ip, jlib:jid_to_string(To), Stanza],
	?INFO_MSG("####rest ::> ~n\tfrom=~p;~n\tip=~p;~n\tto=~p;~n\tpacket=~p;", Log),
    	try
			{xmlel,<<"message">>, _Attrs, _Kids} = Stanza,
			sysn_send:http_send({From,To,Stanza}),
			{200, [], "Ok"}
    	catch
			error:{badmatch, _} -> {406, [], "Error: can only accept <message/>"};
	  		error:{Reason, _} -> {500, [], "Error: " ++ atom_to_list(Reason)}
    	end;
process(_Path, _Request) ->
    	?DEBUG("Got request to ~p: ~p", [_Path, _Request]),
    	{200, [], "Try POSTing a stanza."}.



is_offline(#jid{user=User, server=Server, resource=Resource}) ->
	try
		LUser = jlib:nodeprep(User),
		LServer = jlib:nameprep(Server),
		LResource = jlib:resourceprep(Resource),
		USR = {LUser, LServer, LResource},
		case mnesia:dirty_index_read(session, USR, #session.usr) of
		[] ->
			?ERROR_MSG("mod_rest__is_offline_true usr=~p",[USR]),
			true;
		O ->
			?ERROR_MSG("mod_rest__is_offline_false usr=~p ; session=~p",[USR,O]),
			false
		end 
	catch 
		_:_ ->
			Error = erlang:get_stacktrace(),
			?ERROR_MSG("mod_rest__is_offline Error=~p",[Error]),
			false	
	end.

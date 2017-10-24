-module(aa_push_msg).
-compile(export_all).

get_user_is_online()->
	%%TODO
	true.

register_msg({From, To, Stanza})->
	SystemPush = jlib:string_to_jid(fxml:get_tag_attr_s(<<"systempush">>, Stanza)),
	Data = jlib:string_to_jid(fxml:get_tag_attr_s(<<"data">>, Stanza)),
	Time = jlib:string_to_jid(fxml:get_tag_attr_s(<<"time">>, Stanza)),
	

	[].		

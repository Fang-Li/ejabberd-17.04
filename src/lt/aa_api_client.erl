-module(aa_api_client).
-compile([export_all]).

-define(HTTP_HEAD,"application/x-www-form-urlencoded").
-include("include/ejabberd.hrl").
-include("include/jlib.hrl").
-include("include/logger.hrl").


http_send(Domain, Method, JsonBody)->
  HTTPTarget =  ejabberd_config:get_option({http_server,Domain},fun(V)->V end),
  Form = "body="++type_util:to_list(JsonBody),
  case httpc:request(Method, {type_util:to_list(HTTPTarget), [], ?HTTP_HEAD,Form}, [], []) of
    {ok,{_, _, Res}}->
      ?INFO_MSG("http req:~p response body :~p",[JsonBody, Res]),
      StructJson = jiffy:decode(list_to_binary(Res)),
      {ok,StructJson};
    _R->
      ?ERROR_MSG("http send ~p", [_R])
  end.

get_user_entry(Domain, Uid)->
  {M,S,SS} = now(),
  SN = erlang:integer_to_list(M*1000000000000+S*1000000+SS),
  ParamStruct = {[ {sn,list_to_binary(SN)}, 
                    {service,<<"ejabberd">>}, 
                    {method,<<"get_user_info">>},
                    {params,{[
                          {uid, type_util:to_binary(Uid)}
                        ]}}
                  ]}, 
  Json = jiffy:encode(ParamStruct),
  case http_send(Domain, post, Json) of
    {ok, StructJson}->
      {ok, StructJson};
    _->
      {error, []}
  end.


get_blacklist_status(Domain, Uid, Buid, BDomain)->
  {M,S,SS} = now(),
  SN = erlang:integer_to_list(M*1000000000000+S*1000000+SS),
  Param = {[ {<<"sn">>, list_to_binary(SN)}, 
                    {<<"service">>, <<"ejabberd">>}, 
                    {<<"method">>, <<"get_black_status">>},
                    {<<"params">>, {[
                          {<<"uid">>, type_util:to_binary(Uid)},
                          {<<"buid">>, type_util:to_binary(Buid)},
                          {<<"domain">>, type_util:to_binary(BDomain)}
                        ]}}
                  ]}, 
  Json = jiffy:encode(Param),
  case http_send(Domain, post, Json) of
    {ok, StructJson}->
      {ok, StructJson};
    R->
      ?ERROR_MSG("error aa_api_client:http_send(~p,post,~p)~nResponse=~p",[Domain, Param,R]),
      {error, []}
  end.


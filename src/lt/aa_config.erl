-module(aa_config).

-include("include/ejabberd.hrl").
-include("include/jlib.hrl").
-include("include/logger.hrl").

-compile(export_all).


%%  config mod ||     {Domain,[{Config1,Value1}, {Config2, Value2}, {Config3, Value3}]}

push_config(Params)->
  ?DEBUG("pushconfig ~p",[Params]),
  Hosts = ej:get( {<<"hosts">>},Params),
  FullHosts = save_host_config(Hosts, Params, Hosts),

%  ejabberd_config:add_global_option(hosts, FullHosts),

  ?DEBUG("Hosts ~p",[FullHosts]),
  ok.

save_host_config([], _Params, ResList)->
  ResList;
save_host_config([Host|LastHost], Params, ResList)->
  try
    Hobj = ej:get({type_util:to_binary(Host)},Params),
    GroupHost = ej:get({<<"group_hosts">>},Hobj),
    NewResList = lists:append(ResList, binary_to_list(GroupHost)),
    save_local_config(Host, Hobj),
    save_host_config(LastHost, Params, NewResList)
  catch
    E:R->
      ?ERROR_MSG("save_host_config fail ~p",[{E,R,erlang:get_stacktrace()}]),
      save_host_config(LastHost, Params, ResList)
  end.
    

save_local_config(Host, Hobj)->
  {obj,ConfigList} = Hobj,
  lists:foreach(fun({ConfigKey,ConfigValue})->
        case type_util:to_atom(ConfigKey) of
          http_server->
            ejabberd_config:add_local_option({type_util:to_atom(ConfigKey), type_util:to_list(Host)}, type_util:to_list(ConfigValue));
          _->
            ejabberd_config:add_local_option({type_util:to_atom(ConfigKey), type_util:to_list(Host)}, ConfigValue)
        end
      end,ConfigList).


enable_blacklist(Domain)->
  ejabberd_config:get_option({blacklist_enable,Domain},fun(V)->V end) =:= true.

enable_shielding(Domain)->
  ejabberd_config:get_option({shielding_enable, Domain},fun(V)->V end) =:= true.

enable_planning_system(Domain)->
  ejabberd_config:get_option({planning_system_enable, Domain},fun(V)->V end) =:= true.

enable_friend(Domain, TDomain)->
  case ejabberd_config:get_option({friend_enable, Domain},fun(V)->V end) of
    true->
      case Domain =:= TDomain of
        true->
          true;
        _->
          case enable_cross_domain(Domain, TDomain) of
            true->
              enable_friend_cross(Domain) and enable_friend_cross(TDomain);
            _->
              false
          end
      end;
    _->
      false
  end.

enable_auth(Domain)->
  ejabberd_config:get_option({auth_enable, Domain},fun(V)->V end) =:= true.

enable_ack_from(Domain)->
  ejabberd_config:get_option({ack_from, Domain},fun(V)->V end) =:= true.

enable_group_cache(Domain)->
  ejabberd_config:get_option({group_cache_enable, Domain},fun(V)->V end) =:= true.

enable_cross_domain(Domain,ToDomain) when Domain == ToDomain ->
  true;
enable_cross_domain(Domain,ToDomain) -> 
  case ejabberd_config:get_option({cross_domain, Domain},fun(V)->V end) of
    true->
      case ejabberd_config:get_option({cross_domain_list, Domain},fun(V)->V end) of
        []->
          true;
        DomainList->
          lists:member(type_util:to_binary(ToDomain), DomainList)
      end;
    _->
      false
  end.

enable_group_cross_domain(Domain)->
  ejabberd_config:get_option({group_cross_domain, Domain},fun(V)->V end) =:= true.

enable_super_group_cross_domain(Domain)->
  ejabberd_config:get_option({super_group_cross_domain, Domain},fun(V)->V end) =:= true.


enable_group(Domain)->
  ejabberd_config:get_option({group_enable, Domain},fun(V)->V end) =:= true.

enable_super_group(Domain)->
  ejabberd_config:get_option({super_group_enable, Domain},fun(V)->V end) =:= true.

enable_friend_cross(Domain)->
  ejabberd_config:get_option({friend_cross_enable, Domain},fun(V)->V end) =:= true.



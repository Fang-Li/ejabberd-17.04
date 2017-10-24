-module(aa_sheild_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	AAShield ={ aa_shielding_manage,{aa_shielding_manage, start_link, []}, permanent, brutal_kill, worker, [aa_shielding_manage] },
	{ok, {{one_for_one, 4, 3600}, [AAShield]}}.
%%%===================================================================
%%% Internal functions
%%%===================================================================

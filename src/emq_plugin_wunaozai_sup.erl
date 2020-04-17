
-module(emq_plugin_wunaozai_sup).

-behaviour(supervisor).

-include("emq_plugin_wunaozai.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, Server} = application:get_env(?APP, server),
    PoolSpec = ecpool:pool_spec(?APP, ?APP, emq_plugin_wunaozai_cli, Server),
    {ok, { {one_for_one, 10, 100}, [PoolSpec]} }.


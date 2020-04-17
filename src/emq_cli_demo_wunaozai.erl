
-module(emq_cli_demo_wunaozai).

-include("emq_plugin_wunaozai.hrl").

-include_lib("emqttd/include/emqttd_cli.hrl").

-export([cmd/1]).

cmd(["arg1", "arg2"]) ->
    ?PRINT_MSG("ok");

cmd(_) ->
    ?USAGE([{"cmd arg1 arg2",  "cmd demo"}]).


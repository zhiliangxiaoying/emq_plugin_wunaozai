
-module(emq_plugin_wunaozai_cli).

-behaviour(ecpool_worker).

-include("emq_plugin_wunaozai.hrl").

-include_lib("emqttd/include/emqttd.hrl").

-define(ENV(Key, Opts), proplists:get_value(Key, Opts)).

-export([connect/1, q/2]).

%%--------------------------------------------------------------------
%%%% Redis Connect/Query
%%%%%--------------------------------------------------------------------

connect(Opts) ->
    %T = ?ENV(host, Opts),
    %io:format("~n~n~n Redis Connect ~w ~n~n~n", [T]),
    eredis:start_link(?ENV(host, Opts),
		      ?ENV(port, Opts),
		      ?ENV(database, Opts),
		      ?ENV(password, Opts),
		      1000). % no_reconnect

%% Redis Query.
-spec(q(string(), mqtt_client()) -> {ok, undefined | binary() | list()} | {error, atom() | binary()}).
q(CmdStr, Client) ->
    Cmd = string:tokens(replvar(CmdStr, Client), " "),
    io:format(" CMD: ~s ~n", [Cmd]),
    ecpool:with_client(?APP, fun(C) -> eredis:q(C, Cmd) end).

replvar(Cmd, #mqtt_client{client_id = ClientId, username = Username}) ->
    replvar(replvar(Cmd, "%u", Username), "%c", ClientId).

replvar(S, _Var, undefined) ->
    S;
replvar(S, Var, Val) ->
    re:replace(S, Var, Val, [{return, list}]).


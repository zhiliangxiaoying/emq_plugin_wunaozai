
-module (emq_plugin_wunaozai_config).

-include("emq_plugin_wunaozai.hrl").

-export ([register/0, unregister/0]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
register() ->
    clique_config:load_schema([code:priv_dir(?APP)], ?APP),
    register_formatter(),
    register_config().

unregister() ->
    unregister_formatter(),
    unregister_config(),
    clique_config:unload_schema(?APP).

%%--------------------------------------------------------------------
%% Get ENV Register formatter
%%--------------------------------------------------------------------
register_formatter() ->
    [clique:register_formatter(cuttlefish_variable:tokenize(Key), 
     fun formatter_callback/2) || Key <- keys()].

formatter_callback([_, _, "server"], Params) ->
    lists:concat([proplists:get_value(host, Params), ":", proplists:get_value(port, Params)]);
formatter_callback([_, _, "pool"], Params) ->
    proplists:get_value(pool_size, Params);
formatter_callback([_, _, "password_hash"], Params) when is_atom(Params) ->
    Params;
formatter_callback([_, _, "password_hash"], Params) when is_tuple(Params) ->
    format(tuple_to_list(Params));
formatter_callback([_, _, Key], Params) ->
    proplists:get_value(list_to_atom(Key), Params).

%%--------------------------------------------------------------------
%% UnRegister formatter
%%--------------------------------------------------------------------
unregister_formatter() ->
    [clique:unregister_formatter(cuttlefish_variable:tokenize(Key)) || Key <- keys()].

%%--------------------------------------------------------------------
%% Set ENV Register Config
%%--------------------------------------------------------------------
register_config() ->
    Keys = keys(),
    [clique:register_config(Key , fun config_callback/2) || Key <- Keys],
    clique:register_config_whitelist(Keys, ?APP).

config_callback([_, _, "server"], Value0) ->
    {Host, Port} = parse_servers(Value0),
    {ok, Env} = application:get_env(?APP, server),
    Env1 = lists:keyreplace(host, 1, Env, {host, Host}),
    Env2 = lists:keyreplace(port, 1, Env1, {port, Port}),
    application:set_env(?APP, server, Env2),
    " successfully\n";
config_callback([_, _, "pool"], Value) ->
    {ok, Env} = application:get_env(?APP, server),
    application:set_env(?APP, server, lists:keyreplace(pool_size, 1, Env, {pool_size, Value})),
    " successfully\n";
config_callback([_, _, "password_hash"], Value0) ->
    Value = parse_password_hash(Value0),
    application:set_env(?APP, password_hash, Value),
    " successfully\n";
config_callback([_, _, Key0], Value) ->
    Key = list_to_atom(Key0),
    {ok, Env} = application:get_env(?APP, server),
    application:set_env(?APP, server, lists:keyreplace(Key, 1, Env, {Key, Value})),
    " successfully\n".

%%--------------------------------------------------------------------
%% UnRegister config
%%--------------------------------------------------------------------
unregister_config() ->
    Keys = keys(),
    [clique:unregister_config(Key) || Key <- Keys],
    clique:unregister_config_whitelist(Keys, ?APP).

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------
keys() ->
    ["wunaozai.auth.redis.server",
     "wunaozai.auth.redis.pool",
     "wunaozai.auth.redis.password",
     "wunaozai.auth.redis.database",
     "wunaozai.auth.redis.password_hash"
    ].

format(Value) ->
    format(Value, "").
format([Head], Acc) ->
    lists:concat([Acc, Head]);
format([Head | Tail], Acc) ->
    format(Tail, Acc ++ lists:concat([Head, ","])).

parse_password_hash(Value) ->
    case string:tokens(Value, ",") of
          [Hash]           -> list_to_atom(Hash);
          [Prefix, Suffix] -> {list_to_atom(Prefix), list_to_atom(Suffix)};
          [Hash, MacFun, Iterations, Dklen] -> {list_to_atom(Hash), 
                                                list_to_atom(MacFun), 
                                                list_to_integer(Iterations), 
                                                list_to_integer(Dklen)};
          _                -> plain
    end.

parse_servers(Value) ->
    case string:tokens(Value, ":") of
        [Domain]       -> {Domain, 3306};
        [Domain, Port] -> {Domain, list_to_integer(Port)}
    end.

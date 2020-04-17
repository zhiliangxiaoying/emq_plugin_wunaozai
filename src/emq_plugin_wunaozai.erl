
-module(emq_plugin_wunaozai).

-include("emq_plugin_wunaozai.hrl").

-include_lib("emqttd/include/emqttd.hrl").

-export([load/1, unload/0]).

%% Hooks functions

-export([on_client_connected/3, on_client_disconnected/3]).

-export([on_client_subscribe/4, on_client_unsubscribe/4]).

-export([on_session_created/3, on_session_subscribed/4, on_session_unsubscribed/4, on_session_terminated/4]).

-export([on_message_publish/2, on_message_delivered/4, on_message_acked/4]).

%% Called when the plugin application start
load(Env) ->
    ekaf_init([Env]),
    emqttd:hook('client.connected', fun ?MODULE:on_client_connected/3, [Env]),
    emqttd:hook('client.disconnected', fun ?MODULE:on_client_disconnected/3, [Env]),
    emqttd:hook('client.subscribe', fun ?MODULE:on_client_subscribe/4, [Env]),
    emqttd:hook('client.unsubscribe', fun ?MODULE:on_client_unsubscribe/4, [Env]),
    emqttd:hook('session.created', fun ?MODULE:on_session_created/3, [Env]),
    emqttd:hook('session.subscribed', fun ?MODULE:on_session_subscribed/4, [Env]),
    emqttd:hook('session.unsubscribed', fun ?MODULE:on_session_unsubscribed/4, [Env]),
    emqttd:hook('session.terminated', fun ?MODULE:on_session_terminated/4, [Env]),
    emqttd:hook('message.publish', fun ?MODULE:on_message_publish/2, [Env]),
    emqttd:hook('message.delivered', fun ?MODULE:on_message_delivered/4, [Env]),
    emqttd:hook('message.acked', fun ?MODULE:on_message_acked/4, [Env]),
    io:format("start wunaozai Test Reload.~n", []).

on_client_connected(ConnAck, Client = #mqtt_client{client_id = ClientId}, _Env) ->
    io:format("client ~s connected, connack: ~w~n", [ClientId, ConnAck]),
    ekaf_send(<<"connected">>, ClientId, {}, _Env),
    {ok, Client}.

on_client_disconnected(Reason, _Client = #mqtt_client{client_id = ClientId}, _Env) ->
    io:format("client ~s disconnected, reason: ~w~n", [ClientId, Reason]),
    ekaf_send(<<"disconnected">>, ClientId, {}, _Env),
    ok.

on_client_subscribe(ClientId, Username, TopicTable, _Env) ->
    io:format("client(~s/~s) will subscribe: ~p~n", [Username, ClientId, TopicTable]),
    {ok, TopicTable}.
    
on_client_unsubscribe(ClientId, Username, TopicTable, _Env) ->
    io:format("client(~s/~s) unsubscribe ~p~n", [ClientId, Username, TopicTable]),
    {ok, TopicTable}.

on_session_created(ClientId, Username, _Env) ->
    io:format("session(~s/~s) created.", [ClientId, Username]).

on_session_subscribed(ClientId, Username, {Topic, Opts}, _Env) ->
    io:format("session(~s/~s) subscribed: ~p~n", [Username, ClientId, {Topic, Opts}]),
    ekaf_send(<<"subscribed">>, ClientId, {Topic, Opts}, _Env),
    {ok, {Topic, Opts}}.

on_session_unsubscribed(ClientId, Username, {Topic, Opts}, _Env) ->
    io:format("session(~s/~s) unsubscribed: ~p~n", [Username, ClientId, {Topic, Opts}]),
    ekaf_send(<<"unsubscribed">>, ClientId, {Topic, Opts}, _Env),
    ok.

on_session_terminated(ClientId, Username, Reason, _Env) ->
    io:format("session(~s/~s) terminated: ~p.~n", [ClientId, Username, Reason]),
    stop.

%% transform message and return
on_message_publish(Message = #mqtt_message{topic = <<"$SYS/", _/binary>>}, _Env) ->
    {ok, Message};
on_message_publish(Message, _Env) ->
    io:format("publish ~s~n", [emqttd_message:format(Message)]),
    ekaf_send(<<"public">>, {}, Message, _Env),
    {ok, Message}.

on_message_delivered(ClientId, Username, Message, _Env) ->
    io:format("delivered to client(~s/~s): ~s~n", [Username, ClientId, emqttd_message:format(Message)]),
    {ok, Message}.

on_message_acked(ClientId, Username, Message, _Env) ->
    io:format("client(~s/~s) acked: ~s~n", [Username, ClientId, emqttd_message:format(Message)]),
    {ok, Message}.

%% Called when the plugin application stop
unload() ->
    emqttd:unhook('client.connected', fun ?MODULE:on_client_connected/3),
    emqttd:unhook('client.disconnected', fun ?MODULE:on_client_disconnected/3),
    emqttd:unhook('client.subscribe', fun ?MODULE:on_client_subscribe/4),
    emqttd:unhook('client.unsubscribe', fun ?MODULE:on_client_unsubscribe/4),
    emqttd:unhook('session.created', fun ?MODULE:on_session_created/3),
    emqttd:unhook('session.subscribed', fun ?MODULE:on_session_subscribed/4),
    emqttd:unhook('session.unsubscribed', fun ?MODULE:on_session_unsubscribed/4),
    emqttd:unhook('session.terminated', fun ?MODULE:on_session_terminated/4),
    emqttd:unhook('message.publish', fun ?MODULE:on_message_publish/2),
    emqttd:unhook('message.delivered', fun ?MODULE:on_message_delivered/4),
    emqttd:unhook('message.acked', fun ?MODULE:on_message_acked/4).

%% ==================== ekaf_init STA.===============================%%
ekaf_init(_Env) ->
    % clique 方式读取配置文件
    Env = application:get_env(?APP, kafka),
    {ok, Kafka} = Env,
    Host = proplists:get_value(host, Kafka),
    Port = proplists:get_value(port, Kafka),
    Broker = {Host, Port},
    Topic = proplists:get_value(topic, Kafka),
    io:format("~w ~w ~w ~n", [Host, Port, Topic]),

    % init kafka
    application:set_env(ekaf, ekaf_partition_strategy, strict_round_robin),
    application:set_env(ekaf, ekaf_bootstrap_broker, Broker),
    application:set_env(ekaf, ekaf_bootstrap_topics, list_to_binary(Topic)),
    %application:set_env(ekaf, ekaf_bootstrap_broker, {"127.0.0.1", 9092}),
    %application:set_env(ekaf, ekaf_bootstrap_topics, <<"test">>),

    %io:format("Init ekaf with ~s:~b~n", [Host, Port]),
    %%ekaf:produce_async_batched(<<"test">>, list_to_binary(Json)),
    ok.
%% ==================== ekaf_init END.===============================%%


%% ==================== ekaf_send STA.===============================%%
ekaf_send(Type, ClientId, {}, _Env) ->
    Json = mochijson2:encode([
        		      {type, Type},
        		      {client_id, ClientId},
        		      {message, {}},
        		      {cluster_node, node()},
        		      {ts, emqttd_time:now_ms()}
        		     ]),
    ekaf_send_sync(Json);
ekaf_send(Type, ClientId, {Reason}, _Env) ->
    Json = mochijson2:encode([
			      {type, Type},
			      {client_id, ClientId},
			      {cluster_node, node()},
			      {message, Reason},
			      {ts, emqttd_time:now_ms()}
			     ]),
    ekaf_send_sync(Json);
ekaf_send(Type, ClientId, {Topic, Opts}, _Env) ->
    Json = mochijson2:encode([
			      {type, Type},
			      {client_id, ClientId},
			      {cluster_node, node()},
			      {message, [
					 {topic, Topic},
					 {opts, Opts}
					]},
			      {ts, emqttd_time:now_ms()}
			     ]),
    ekaf_send_sync(Json);
ekaf_send(Type, _, Message, _Env) ->
    Id = Message#mqtt_message.id,
    From = Message#mqtt_message.from, %需要登录和不需要登录这里的返回值是不一样的
    Topic = Message#mqtt_message.topic,
    Payload = Message#mqtt_message.payload,
    Qos = Message#mqtt_message.qos,
    Dup = Message#mqtt_message.dup,
    Retain = Message#mqtt_message.retain,
    Timestamp = Message#mqtt_message.timestamp,

    ClientId = c(From),
    Username = u(From),

    Json = mochijson2:encode([
			      {type, Type},
			      {client_id, ClientId},
			      {message, [
					 {username, Username},
					 {topic, Topic},
					 {payload, Payload},
					 {qos, i(Qos)},
					 {dup, i(Dup)},
					 {retain, i(Retain)}
					]},
			      {cluster_node, node()},
			      {ts, emqttd_time:now_ms()}
			     ]),
    ekaf_send_sync(Json).

ekaf_send_async(Msg) ->
    Topic = ekaf_get_topic(),
    ekaf_send_async(Topic, Msg).
ekaf_send_async(Topic, Msg) ->
    ekaf:produce_async_batched(list_to_binary(Topic), list_to_binary(Msg)).
ekaf_send_sync(Msg) ->
    Topic = ekaf_get_topic(),
    ekaf_send_sync(Topic, Msg).
ekaf_send_sync(Topic, Msg) ->
    ekaf:produce_sync_batched(list_to_binary(Topic), list_to_binary(Msg)).

i(true) -> 1;
i(false) -> 0;
i(I) when is_integer(I) -> I.
c({ClientId, Username}) -> ClientId;
c(From) -> From.
u({ClientId, Username}) -> Username;
u(From) -> From.
%% ==================== ekaf_send END.===============================%%


%% ==================== ekaf_set_host STA.===============================%%
ekaf_set_host(Host) ->
    ekaf_set_host(Host, 9092).
ekaf_set_host(Host, Port) ->
    Broker = {Host, Port},
    application:set_env(ekaf, ekaf_bootstrap_broker, Broker),
    io:format("reset ekaf Broker ~s:~b ~n", [Host, Port]),
    ok.
%% ==================== ekaf_set_host END.===============================%%

%% ==================== ekaf_set_topic STA.===============================%%
ekaf_set_topic(Topic) ->
    application:set_env(ekaf, ekaf_bootstrap_topics, list_to_binary(Topic)),
    ok.
ekaf_get_topic() ->
    Env = application:get_env(?APP, kafka),
    {ok, Kafka} = Env,
    Topic = proplists:get_value(topic, Kafka),
    Topic.
%% ==================== ekaf_set_topic END.===============================%%

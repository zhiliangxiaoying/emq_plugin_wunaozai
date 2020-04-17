-module(emq_acl_demo_wunaozai).

-behaviour(emqttd_acl_mod).

-include("emq_plugin_wunaozai.hrl").

-include_lib("emqttd/include/emqttd.hrl").

%% ACL callbacks
-export([init/1, check_acl/2, reload_acl/1, description/0]).

-record(state, {acl_cmd}).

init(AclCmd) ->
    {ok, #state{acl_cmd = AclCmd}}.

check_acl({#mqtt_client{username = <<$$, _/binary>>}, _PubSub, _Topic}, _State) ->
    %io:format("~n TEST ~n", []),
    ignore;

check_acl({Client, PubSub, Topic}, #state{acl_cmd     = AclCmd}) ->
    io:format("ACL: ~s ~s ~s ~n", [PubSub, Topic, AclCmd]),
    case emq_plugin_wunaozai_cli:q(AclCmd, Client) of
	%{ok, []}         -> ignore;
	{ok, []}         -> deny;
	{ok, Rules}      -> case match(Client, PubSub, Topic, Rules) of
				allow   -> allow;
				nomatch -> deny
			    end;
	%{error, Reason} -> lager:error("Redis check_acl error: ~p~n", [Reason]), ignore
	{error, Reason} -> lager:error("Redis check_acl error: ~p~n", [Reason]), deny
    end.

match(_Client, _PubSub, _Topic, []) ->
    nomatch;
match(Client, PubSub, Topic, [Filter, Access | Rules]) ->
    case {match_topic(Topic, feed_var(Client, Filter)), match_access(PubSub, b2i(Access))} of
	{true, true} -> allow;
	{_, _} -> match(Client, PubSub, Topic, Rules)
    end.

match_topic(Topic, Filter) ->
    emqttd_topic:match(Topic, Filter).

match_access(subscribe, Access) ->
    (1 band Access) > 0;
match_access(publish, Access) ->
    (2 band Access) > 0.

feed_var(#mqtt_client{client_id = ClientId, username = Username}, Str) ->
    lists:foldl(fun({Var, Val}, Acc) ->
			feed_var(Acc, Var, Val)
		end, Str, [{"%u", Username}, {"%c", ClientId}]).

feed_var(Str, _Var, undefined) ->
    Str;
feed_var(Str, Var, Val) ->
    re:replace(Str, Var, Val, [global, {return, binary}]).

b2i(Bin) -> list_to_integer(binary_to_list(Bin)).

reload_acl(_State) -> ok.

description() -> "Redis ACL Module".


%init(Opts) ->
%    {ok, Opts}.
%
%check_acl({Client, PubSub, Topic}, _Opts) ->
%    io:format("ACL Demo: ~p ~p ~p~n", [Client, PubSub, Topic]),
%    io:format("~n == ACL ==~n"),
%    if
%	Topic == <<"/World">> ->
%	    io:format("allow"),
%	    allow;
%	true ->
%	    io:format("deny"),
%	    deny
%    end.
%
%reload_acl(_Opts) ->
%    ok.
%
%description() -> "ACL Demo Module".



-module(emq_auth_demo_wunaozai).

-behaviour(emqttd_auth_mod).

-include("emq_plugin_wunaozai.hrl").

-include_lib("emqttd/include/emqttd.hrl").

-export([init/1, check/3, description/0]).

-define(UNDEFINED(S), (S =:= undefined)).

-record(state, {auth_cmd, super_cmd, hash_type}).

init({AuthCmd, SuperCmd, HashType}) -> 
    {ok, #state{auth_cmd = AuthCmd, super_cmd = SuperCmd, hash_type = HashType}}.

check(#mqtt_client{username = Username}, Password, _State)
  when ?UNDEFINED(Username); ?UNDEFINED(Password) ->
    {error, username_or_password_undefined};

check(Client, Password, #state{auth_cmd  = AuthCmd,
			       super_cmd = SuperCmd,
			       hash_type = HashType}) ->
    %io:format("~n~n ~s ~n~s ~n~s ~n~s ~n~n", [AuthCmd, SuperCmd, HashType, Client]),
    Result = case emq_plugin_wunaozai_cli:q(AuthCmd, Client) of
		 {ok, PassHash} when is_binary(PassHash) ->
		     check_pass(PassHash, Password, HashType);  
		 {ok, [undefined|_]} ->
		     ignore;
		 {ok, [PassHash]} ->
		     check_pass(PassHash, Password, HashType);
		 {ok, [PassHash, Salt|_]} ->
		     check_pass(PassHash, Salt, Password, HashType);
		 {error, Reason} ->
		     {error, Reason}
	     end,
    case Result of 
	ok -> {ok, is_superuser(SuperCmd, Client)}; 
	Error -> Error 
    end.
check_pass(PassHash, Password, HashType) ->
        check_pass(PassHash, hash(HashType, Password)).
check_pass(PassHash, Salt, Password, {pbkdf2, Macfun, Iterations, Dklen}) ->
      check_pass(PassHash, hash(pbkdf2, {Salt, Password, Macfun, Iterations, Dklen}));
check_pass(PassHash, Salt, Password, {salt, bcrypt}) ->
        check_pass(PassHash, hash(bcrypt, {Salt, Password}));
check_pass(PassHash, Salt, Password, {salt, HashType}) ->
        check_pass(PassHash, hash(HashType, <<Salt/binary, Password/binary>>));
check_pass(PassHash, Salt, Password, {HashType, salt}) ->
        check_pass(PassHash, hash(HashType, <<Password/binary, Salt/binary>>)).

check_pass(PassHash, PassHash) -> ok;
check_pass(_, _)               -> {error, password_error}.

description() -> "Authentication with Redis".

hash(Type, Password) -> emqttd_auth_mod:passwd_hash(Type, Password).

-spec(is_superuser(undefined | list(), mqtt_client()) -> boolean()).
is_superuser(undefined, _Client) ->
    false;
is_superuser(SuperCmd, Client) ->
    case emq_plugin_wunaozai_cli:q(SuperCmd, Client) of
	{ok, undefined} -> false;
	{ok, <<"1">>}   -> true;
	{ok, _Other}    -> false;
	{error, _Error} -> false
    end.



%check(#mqtt_client{client_id = ClientId, username = Username}, Password, _Opts) ->
%    io:format("Auth Demo: clientId=~p, username=~p, password=~p~n",
%              [ClientId, Username, Password]),
%    if	
%	Username == <<"test001">> ->
%	    ok;
%	true ->
%	    error
%    end.
%
% description() -> "Auth Demo Module".



-module(emq_plugin_wunaozai_app).

-behaviour(application).

-include("emq_plugin_wunaozai.hrl").

%% Application callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = emq_plugin_wunaozai_sup:start_link(),
    if_cmd_enabled(auth_cmd, fun reg_authmod/1),
    if_cmd_enabled(acl_cmd, fun reg_aclmod/1),
    emq_plugin_wunaozai:load(application:get_all_env()),
    emq_plugin_wunaozai_config:register(), % 这里使用config模块的注册方法
    {ok, Sup}.

stop(_State) ->
    emqttd_access_control:unregister_mod(auth, emq_auth_demo_wunaozai),
    emqttd_access_control:unregister_mod(acl, emq_acl_demo_wunaozai),
    emq_plugin_wunaozai:unload(),
    emq_plugin_wunaozai_config:unregister(). % 这里使用config模块的卸载方法
    

%% 根据具体配置文件 emq_plugin_wunaozai.conf 是否有auth_cmd 或者 acl_cmd 配置项目来动态加载所属模块

reg_authmod(AuthCmd) ->
    SuperCmd = application:get_env(?APP, super_cmd, undefined),
    {ok, PasswdHash} = application:get_env(?APP, password_hash),
    emqttd_access_control:register_mod(auth, emq_auth_demo_wunaozai, {AuthCmd, SuperCmd, PasswdHash}).

reg_aclmod(AclCmd) ->
    emqttd_access_control:register_mod(acl, emq_acl_demo_wunaozai, AclCmd).

if_cmd_enabled(Par, Fun) ->
    case application:get_env(?APP, Par) of
	{ok, Cmd} -> Fun(Cmd);
	undefined -> ok
    end.

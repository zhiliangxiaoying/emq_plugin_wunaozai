PROJECT = emq_plugin_wunaozai
PROJECT_DESCRIPTION = EMQ Plugin Wunaozai Test
PROJECT_VERSION = 2.3.1

DEPS = eredis ecpool clique ekaf

dep_eredis = git https://github.com/wooga/eredis master
dep_ecpool = git https://github.com/emqtt/ecpool master
dep_clique = git https://github.com/emqtt/clique
dep_ekaf   = git https://github.com/helpshift/ekaf master

BUILD_DEPS = emqttd cuttlefish
dep_emqttd = git https://github.com/emqtt/emqttd master
dep_cuttlefish = git https://github.com/emqtt/cuttlefish

ERLC_OPTS += +debug_info
ERLC_OPTS += +'{parse_transform, lager_transform}'

NO_AUTOPATCH = cuttlefish

COVER = true

include erlang.mk

app:: rebar.config

app.config::
	deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emq_plugin_wunaozai.conf -i priv/emq_plugin_wunaozai.schema -d data

copy:
	cp -r ./ebin ../../_rel/emqttd/lib/emq_plugin_wunaozai-2.3.1

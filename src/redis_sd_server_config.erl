%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2014, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  29 Aug 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redis_sd_server_config).

-include("redis_sd_server.hrl").

%% API
-export([list_to_service/1, list_to_service/2]).

%% Internal
-export([opt/2, opt/3, req/2]).
-ignore_xref([opt/2, opt/3, req/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec list_to_service([{atom(), term()}]) -> redis_sd_service().
list_to_service(ServiceConfig) ->
	list_to_service([], ServiceConfig).

-spec list_to_service([module()], [{atom(), term()}]) -> redis_sd_service().
list_to_service(Apps, ServiceConfig) when is_list(Apps) ->
	Default = ?REDIS_SD_SERVICE{},
	Defaults = [
		{enabled, Default?REDIS_SD_SERVICE.enabled, app},
		{redis_opts, Default?REDIS_SD_SERVICE.redis_opts, app},
		{redis_auth, Default?REDIS_SD_SERVICE.redis_auth, app},
		{redis_ns, Default?REDIS_SD_SERVICE.redis_ns, app},
		{cmd_auth, Default?REDIS_SD_SERVICE.cmd_auth, app},
		{cmd_del, Default?REDIS_SD_SERVICE.cmd_del, app},
		{cmd_publish, Default?REDIS_SD_SERVICE.cmd_publish, app},
		{cmd_setex, Default?REDIS_SD_SERVICE.cmd_setex, app},
		{min_wait, Default?REDIS_SD_SERVICE.min_wait, app},
		{max_wait, Default?REDIS_SD_SERVICE.max_wait, app}
	],
	S = redis_sd_config:merge(Apps ++ [redis_sd_server], Defaults, ServiceConfig),
	Enabled = opt(enabled, S, Default?REDIS_SD_SERVICE.enabled),
	case Enabled of
		false ->
			ok;
		true ->
			ok;
		BadBoolean ->
			erlang:error({invalid_enabled_boolean, {enabled, BadBoolean}, S})
	end,
	?REDIS_SD_SERVICE{
		enabled = Enabled,

		name     = req(name, S),
		domain   = req(domain, S),
		type     = req(type, S),
		service  = req(service, S),
		instance = req(instance, S),
		ttl      = req(ttl, S),

		%% SRV
		priority = req(priority, S),
		weight   = req(weight, S),
		port     = req(port, S),
		target   = req(target, S),

		%% TXT
		txtdata = opt(txtdata, S, Default?REDIS_SD_SERVICE.txtdata),

		%% Redis Options
		redis_opts = req(redis_opts, S),
		redis_auth = opt(redis_auth, S, Default?REDIS_SD_SERVICE.redis_auth),
		redis_ns   = opt(redis_ns, S, Default?REDIS_SD_SERVICE.redis_ns),

		%% Redis Commands
		cmd_auth    = req(cmd_auth, S),
		cmd_del     = req(cmd_del, S),
		cmd_publish = req(cmd_publish, S),
		cmd_setex   = req(cmd_setex, S),

		%% Reconnect Options
		min_wait = req(min_wait, S),
		max_wait = req(max_wait, S)
	}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
opt(Key, P) ->
	opt(Key, P, undefined).

%% @private
opt(Key, P, Default) ->
	case lists:keyfind(Key, 1, P) of
		false ->
			Default;
		{Key, Value} ->
			Value
	end.

%% @doc Return `Value' for `Key' in proplist `P' or crashes with an
%% informative message if no value is found.
%% @private
req(Key, P) ->
	case lists:keyfind(Key, 1, P) of
		false ->
			erlang:error({missing_required_config, Key, P});
		{Key, undefined} ->
			erlang:error({missing_required_config, Key, P});
		{Key, Value} ->
			Value
	end.

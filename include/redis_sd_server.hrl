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

-ifndef(REDIS_SD_SERVER_HRL).

-include_lib("redis_sd_spec/include/redis_sd.hrl").

-type redis_sd_service_fun(Type) ::
	fun((redis_sd:obj()) -> Type).
-type redis_sd_service_val(Type) ::
	Type | redis_sd_service_fun(Type).

-record(redis_sd_service_v1, {
	enabled = true :: boolean(),

	name     = undefined :: undefined | atom(),
	domain   = undefined :: undefined | redis_sd_service_val(iodata()),
	type     = undefined :: undefined | redis_sd_service_val(iodata()),
	service  = undefined :: undefined | redis_sd_service_val(iodata()),
	instance = undefined :: undefined | redis_sd_service_val(iodata()), % instance._service._type.domain
	ttl      = undefined :: undefined | redis_sd_service_val(non_neg_integer()), % seconds

	%% SRV
	priority = undefined :: undefined | redis_sd_service_val(non_neg_integer()),
	weight   = undefined :: undefined | redis_sd_service_val(non_neg_integer()),
	port     = undefined :: undefined | redis_sd_service_val(inet:port_number()),
	target   = undefined :: undefined | redis_sd_service_val(inet:hostname() | inet:ip_address()),

	%% TXT
	txtdata = [] :: redis_sd_service_val([{redis_sd_service_val(iodata()), redis_sd_service_val(iodata())}]),

	%% Redis Options
	redis_opts = {tcp, ["127.0.0.1", 6379]} :: {tcp | unix, [string() | integer() | timeout()]},
	redis_auth = undefined                  :: undefined | iodata(),
	redis_ns   = ""                         :: iodata(),
	redis_cli  = undefined                  :: undefined | port(),

	%% Redis Commands
	cmd_auth         = "AUTH"         :: iodata(),
	cmd_del          = "DEL"          :: iodata(),
	cmd_publish      = "PUBLISH"      :: iodata(),
	cmd_setex        = "SETEX"        :: iodata(),

	%% Reconnect Options
	min_wait = 1         :: integer(), % seconds
	max_wait = 120       :: integer(), % seconds
	backoff  = undefined :: undefined | backoff:backoff(),
	bref     = undefined :: undefined | reference(),
	aref     = undefined :: undefined | reference(),

	%% Refresh Cache
	rec = undefined :: undefined | redis_sd_dns:rec(),
	ptr = undefined :: undefined | binary(),
	srv = undefined :: undefined | binary(),
	key = undefined :: undefined | binary()
}).

-type redis_sd_service() :: #redis_sd_service_v1{}.

-define(REDIS_SD_SERVICE, #redis_sd_service_v1).

-define(REDIS_SD_SERVER_HRL, 1).

-endif.

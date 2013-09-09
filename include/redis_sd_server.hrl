%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  29 Aug 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------

-include_lib("redis_sd_spec/include/redis_sd.hrl").

-type service_fun(Type) ::
	fun((redis_sd:obj()) -> Type).
-type service_val(Type) ::
	Type | service_fun(Type).

-record(service, {
	name     = undefined :: undefined | atom(),
	domain   = undefined :: undefined | service_val(iodata()),
	type     = undefined :: undefined | service_val(iodata()),
	service  = undefined :: undefined | service_val(iodata()),
	instance = undefined :: undefined | service_val(iodata()), % instance._service._type.domain
	ttl      = undefined :: undefined | service_val(non_neg_integer()), % seconds

	%% SRV
	priority = undefined :: undefined | service_val(non_neg_integer()),
	weight   = undefined :: undefined | service_val(non_neg_integer()),
	port     = undefined :: undefined | service_val(inet:port_number()),
	target   = undefined :: undefined | service_val(inet:hostname() | inet:ip_address()),

	%% TXT
	txtdata = [] :: service_val([{service_val(iodata()), service_val(iodata())}]),

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
	zref     = undefined :: undefined | reference(),
	aref     = undefined :: undefined | reference(),

	%% Refresh Cache
	obj = undefined :: undefined | redis_sd:obj(),
	ptr = undefined :: undefined | binary(),
	srv = undefined :: undefined | binary(),
	key = undefined :: undefined | binary()
}).

-type service() :: #service{}.

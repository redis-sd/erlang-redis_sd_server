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

-record(service, {
	name    = undefined :: undefined | atom(),
	service = "generic" :: string(),
	type    = "tcp"     :: string(),
	domain  = "local"   :: string(),
	host    = undefined :: undefined | inet:hostname(),
	port    = undefined :: undefined | inet:port_number(),
	ttl     = 120       :: integer(), % seconds
	txtdata = []        :: [{iodata(), iodata()}],

	%% Redis Options
	redis_opts = {tcp, ["127.0.0.1", 6379]} :: {tcp | unix, [string() | integer() | timeout()]},
	redis_ns   = <<>>						:: binary() | string(),
	redis_cli  = undefined                  :: undefined | port(),

	%% Reconnect Options
	min_wait = 1         :: integer(), % seconds
	max_wait = 120       :: integer(), % seconds
	backoff  = undefined :: undefined | backoff:backoff(),
	bref     = undefined :: undefined | reference(),
	aref     = undefined :: undefined | reference(),
	lock     = undefined :: undefined | reference(),

	%% Sync & Announce Cache
	hostname = undefined :: undefined | iodata(),
	hostkey  = undefined :: undefined | iodata(),
	servkey  = undefined :: undefined | iodata(),
	typekey  = undefined :: undefined | iodata()
}).

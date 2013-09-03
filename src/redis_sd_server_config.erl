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
-module(redis_sd_server_config).

-include("redis_sd_server.hrl").

%% API
-export([list_to_service/1]).

%% Internal
-export([opt/2, opt/3, req/2]).
-ignore_xref([opt/2, opt/3, req/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec list_to_service([{atom(), term()}]) -> #service{}.
list_to_service(S) ->
	Default = #service{},
	#service{
		name     = req(name, S),
		service  = opt(service, S, Default#service.service),
		type     = opt(type, S, Default#service.type),
		domain   = opt(domain, S, Default#service.domain),
		hostname = opt(hostname, S, Default#service.hostname),
		ttl      = opt(ttl, S, Default#service.ttl),

		%% SRV
		host = opt(host, S, Default#service.host),
		port = opt(port, S, Default#service.port),

		%% TXT
		txtdata = opt(txtdata, S, Default#service.txtdata),

		%% Redis Options
		redis_opts = opt(redis_opts, S, Default#service.redis_opts),
		redis_ns   = opt(redis_ns, S, Default#service.redis_ns),

		%% Reconnect Options
		min_wait = opt(min_wait, S, Default#service.min_wait),
		max_wait = opt(max_wait, S, Default#service.max_wait)
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
		{Key, Value} ->
			Value
	end.

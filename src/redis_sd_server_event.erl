%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  30 Aug 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redis_sd_server_event).

-include("redis_sd_server.hrl").

-define(MANAGER, redis_sd_server_manager).

%% API
-export([add_handler/2, service_init/1, service_connect/1, service_announce/2, service_terminate/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

add_handler(Handler, Pid) ->
	gen_event:add_handler(?MANAGER, Handler, Pid).

service_init(Service=#service{}) ->
	gen_event:notify(?MANAGER, {service, init, Service}).

service_connect(Service=#service{}) ->
	gen_event:notify(?MANAGER, {service, connect, Service}).

service_announce(Data, Service=#service{}) ->
	gen_event:notify(?MANAGER, {service, announce, Data, Service}).

service_terminate(Reason, Service=#service{}) ->
	gen_event:notify(?MANAGER, {service, terminate, Reason, Service}).

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
-export([manager/0, add_handler/2]).
-export([service_init/1, service_enable/1, service_disable/1,
	service_connect/1, service_announce/2, service_terminate/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

manager() ->
	?MANAGER.

add_handler(Handler, Pid) ->
	gen_event:add_handler(manager(), Handler, Pid).

service_init(Service=?REDIS_SD_SERVICE{}) ->
	notify({service, init, Service}).

service_enable(Service=?REDIS_SD_SERVICE{}) ->
	notify({service, enable, Service}).

service_disable(Service=?REDIS_SD_SERVICE{}) ->
	notify({service, disable, Service}).

service_connect(Service=?REDIS_SD_SERVICE{}) ->
	notify({service, connect, Service}).

service_announce(Data, Service=?REDIS_SD_SERVICE{}) ->
	notify({service, announce, Data, Service}).

service_terminate(Reason, Service=?REDIS_SD_SERVICE{}) ->
	notify({service, terminate, Reason, Service}).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
notify(Message) ->
	gen_event:notify(manager(), Message).

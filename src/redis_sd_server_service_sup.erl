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
-module(redis_sd_server_service_sup).
-behaviour(supervisor).

-include("redis_sd_server.hrl").

%% API
-export([start_link/1, service_sup_name/1, graceful_shutdown/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Service=?REDIS_SD_SERVICE{}) ->
	SupName = service_sup_name(Service),
	supervisor:start_link({local, SupName}, ?MODULE, Service).

service_sup_name(?REDIS_SD_SERVICE{name=Name}) ->
	list_to_atom("redis_sd_server_" ++ atom_to_list(Name) ++ "_service_sup").

%% @doc Gracefully shutdown the named service.
graceful_shutdown(Name) ->
	case catch redis_sd_server_service:graceful_shutdown(Name) of
		ok ->
			ok;
		_ ->
			forced_shutdown
	end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init(Service=?REDIS_SD_SERVICE{}) ->
	ServiceSpec = {redis_sd_server_service,
		{redis_sd_server_service, start_link, [Service]},
		transient, 2000, worker, [redis_sd_server_service]},
	%% five restarts in 60 seconds, then shutdown
	Restart = {one_for_all, 5, 60},
	{ok, {Restart, [ServiceSpec]}}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

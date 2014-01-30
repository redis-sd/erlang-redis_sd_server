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
-module(redis_sd_server_sup).
-behaviour(supervisor).

-include("redis_sd_server.hrl").

%% API
-export([start_link/0, new_service/1, rm_service/1, delete_service/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Create a new service from proplist service config `ServiceConfig'. The
%% public API for this functionality is {@link redis_sd_server:new_service/1}.
new_service(ServiceConfig) when is_list(ServiceConfig) ->
	new_service(redis_sd_server_config:list_to_service(ServiceConfig));
new_service(Service=?REDIS_SD_SERVICE{}) ->
	case redis_sd_server_dns:resolve([service, type, domain], Service) of
		{ok, _} ->
			Spec = service_sup_spec(Service),
			supervisor:start_child(?MODULE, Spec);
		ResolveError ->
			ResolveError
	end.

%% @doc Gracefully shutdown the named service.
rm_service(Name) ->
	case redis_sd_server_service_sup:graceful_shutdown(Name) of
		ok ->
			ok;
		forced_shutdown ->
			delete_service(Name)
	end.

%% @doc Forcefully shutdown the named service.
delete_service(Name) ->
	SupName = service_sup_name(Name),
	case supervisor:terminate_child(?MODULE, SupName) of
		{error, not_found} ->
			ok;
		ok ->
			supervisor:delete_child(?MODULE, SupName);
		Error ->
			Error
	end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
	redis_sd_server = ets:new(redis_sd_server, [ordered_set, public, named_table]),
	ManagerSpec = {redis_sd_server_event:manager(),
		{gen_event, start_link, [{local, redis_sd_server_event:manager()}]},
		permanent, 5000, worker, [gen_event]},
	ServerSpec = {redis_sd_server,
		{redis_sd_server, start_link, []},
		permanent, 5000, worker, [redis_sd_server]},

	%% a list of service configs
	Config = case application:get_env(redis_sd_server, services) of
		{ok, C} ->
			C;
		undefined ->
			[]
	end,
	Services = [redis_sd_server_config:list_to_service(L) || L <- Config],
	ServiceSupSpecs = [service_sup_spec(Service) || Service <- Services],

	%% five restarts in 60 seconds, then shutdown
	Restart = {rest_for_one, 5, 60},
	{ok, {Restart, [ManagerSpec, ServerSpec | ServiceSupSpecs]}}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
service_sup_spec(Service=?REDIS_SD_SERVICE{name=Name}) ->
	SupName = service_sup_name(Name),
	{SupName,
		{redis_sd_server_service_sup, start_link, [Service]},
		transient, 5000, supervisor, [redis_sd_server_service_sup]}.

%% @private
service_sup_name(Name) ->
	list_to_atom("redis_sd_server_" ++ atom_to_list(Name) ++ "_service_sup").

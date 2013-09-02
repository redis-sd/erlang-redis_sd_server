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
-module(redis_sd_server_service).
-behaviour(gen_fsm).

-include("redis_sd_server.hrl").

%% API
-export([start_link/1, get/1, lock/1, set/3, graceful_shutdown/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
	terminate/3, code_change/4]).

-export([connect/2, authorize/2, announce/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @private
start_link(Service=#service{name=Name}) ->
	gen_fsm:start_link({local, Name}, ?MODULE, Service, []).

%% @doc Get the Service record.
get(Name) ->
	gen_fsm:sync_send_all_state_event(Name, get).

%% @doc Lock the Service record with the intention of calling set/3.
%% Previous locks will be overwritten.
lock(Name) ->
	gen_fsm:sync_send_all_state_event(Name, lock).

%% @doc Update the Service record.
set(Name, Lock, Service=#service{}) when is_reference(Lock) ->
	gen_fsm:sync_send_all_state_event(Name, {set, Lock, Service}).

%% @doc Gracefully shutdown the service.
graceful_shutdown(Name) ->
	gen_fsm:sync_send_all_state_event(Name, graceful_shutdown).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%% @private
init(Service=#service{min_wait=MinWait, max_wait=MaxWait}) ->
	redis_sd_server_event:service_init(Service),
	Backoff = backoff:init(timer:seconds(MinWait), timer:seconds(MaxWait), self(), connect),
	BRef = start_connect(initial),
	{ok, connect, Service#service{backoff=Backoff, bref=BRef}}.

%% @private
handle_event(_Event, _StateName, Service) ->
	{stop, badmsg, Service}.

%% @private
handle_sync_event(get, _From, StateName, Service) ->
	Reply = {ok, Service#service{lock=undefined}},
	{reply, Reply, StateName, Service};
handle_sync_event(lock, _From, StateName, Service) ->
	Lock = erlang:make_ref(),
	Reply = {ok, Lock, Service},
	{reply, Reply, StateName, Service#service{lock=Lock}};
handle_sync_event({set, Lock, NewService=#service{}}, _From, StateName, Service) when is_reference(Lock) ->
	case Service#service.lock of
		Lock ->
			Service2 = NewService#service{lock=undefined},
			{reply, {ok, Service2}, StateName, Service2};
		_ ->
			{reply, {error, invalid_lock}, StateName, Service}
	end;
handle_sync_event(graceful_shutdown, _From, _StateName, Service) ->
	{stop, normal, ok, Service};
handle_sync_event(_Event, _From, _StateName, Service) ->
	{stop, badmsg, Service}.

%% @private
handle_info({timeout, BRef, connect}, _StateName, Service=#service{bref=BRef}) ->
	connect(timeout, Service#service{bref=undefined});
handle_info({timeout, ZRef, authorize}, authorize, Service=#service{zref=ZRef}) ->
	authorize(timeout, Service#service{zref=undefined});
handle_info({timeout, ARef, announce}, announce, Service=#service{aref=ARef}) ->
	announce(timeout, Service#service{aref=undefined});
handle_info({redis_error, Client, {Error, Reason}}, StateName, Service=#service{redis_cli=Client, name=Name}) ->
	error_logger:warning_msg(
		"** ~p ~p received redis_error in ~p/~p~n"
		"   for the reason ~p:~p~n",
		[?MODULE, Name, handle_info, 3, Error, Reason]),
	{next_state, StateName, Service};
handle_info({redis_closed, Client}, _StateName, Service=#service{redis_cli=Client}) ->
	ok = stop_announce(Service),
	ok = hierdis_async:close(Client),
	connect(timeout, Service#service{redis_cli=undefined, aref=undefined});
handle_info(Info, StateName, Service=#service{name=Name}) ->
	error_logger:error_msg(
		"** ~p ~p unhandled info in ~p/~p~n"
		"   Info was: ~p~n",
		[?MODULE, Name, handle_info, 3, Info]),
	{next_state, StateName, Service}.

%% @private
terminate(normal, announce, Service=#service{redis_cli=Client}) ->
	catch announce(timeout, Service#service{ttl=0}),
	catch hierdis_async:close(Client),
	redis_sd_server_event:service_terminate(normal, Service),
	ok;
terminate(Reason, _StateName, Service) ->
	redis_sd_server_event:service_terminate(Reason, Service),
	ok.

%% @private
code_change(_OldVsn, StateName, Service, _Extra) ->
	{ok, StateName, Service}.

%%%===================================================================
%%% States
%%%===================================================================

%% @private
connect(timeout, Service=#service{redis_opts={Transport, Args}, backoff=Backoff}) ->
	redis_sd_server_event:service_connect(Service),
	ConnectFun = case Transport of
		tcp ->
			connect;
		unix ->
			connect_unix
	end,
	case erlang:apply(hierdis_async, ConnectFun, Args) of
		{ok, Client} ->
			{_Start, Backoff2} = backoff:succeed(Backoff),
			ZRef = start_authorize(initial),
			{next_state, authorize, Service#service{redis_cli=Client, backoff=Backoff2, zref=ZRef}};
		{error, _ConnectError} ->
			BRef = start_connect(Service),
			{_Delay, Backoff2} = backoff:fail(Backoff),
			{next_state, connect, Service#service{backoff=Backoff2, bref=BRef}}
	end.

%% @private
authorize(timeout, Service=#service{redis_auth=undefined}) ->
	ARef = start_announce(initial),
	{next_state, announce, Service#service{aref=ARef}};
authorize(timeout, Service=#service{redis_auth=Password}) when Password =/= undefined ->
	case redis_auth(Password, Service) of
		ok ->
			ARef = start_announce(initial),
			{next_state, announce, Service#service{aref=ARef}};
		{error, Reason} ->
			{stop, {error, Reason}, Service}
	end.

%% @private
announce(timeout, Service) ->
	case redis_sd_server_dns:refresh(Service) of
		{ok, Data, Service2} ->
			ok = redis_announce(Data, Service2),
			ARef = start_announce(Service2),
			{next_state, announce, Service2#service{aref=ARef}};
		RefreshError ->
			error_logger:warning_msg(
				"** ~p ~p non-fatal error in ~p/~p~n"
				"   for the reason ~p:~p~n",
				[?MODULE, self(), announce, 2, error, RefreshError]),
			ARef = start_announce(Service),
			{next_state, announce, Service#service{aref=ARef}}
	end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
redis_auth(Password, #service{redis_cli=Client, cmd_auth=AUTH}) ->
	Command = [AUTH, Password],
	try hierdis_async:command(Client, Command) of
		{ok, _} ->
			ok;
		{error, Reason} ->
			{error, Reason}
	catch
		Class:Reason ->
			error_logger:error_msg(
				"** ~p ~p terminating in ~p/~p~n"
				"   for the reason ~p:~p~n** Stacktrace: ~p~n~n",
				[?MODULE, self(), redis_auth, 2, Class, Reason, erlang:get_stacktrace()]),
			erlang:error(Reason)
	end.

%% @private
redis_announce(Data, Service=#service{redis_cli=Client, redis_ns=Namespace, ttl=TTL, cmd_del=DEL, cmd_publish=PUBLISH, cmd_setex=SETEX}) ->
	redis_sd_server_event:service_announce(Data, Service),
	Channel = [Namespace, "PTR:", redis_sd:nsreverse(Service#service.typekey)],
	SetOrDel = case TTL of
		0 ->
			[DEL, Channel];
		_ when is_integer(TTL) ->
			[SETEX, Channel, integer_to_list(TTL), Data]
	end,
	Transaction = [SetOrDel, [PUBLISH, Channel, Data]],
	try hierdis_async:transaction(Client, Transaction) of
		{ok, _} ->
			ok;
		{error, Reason} ->
			error_logger:warning_msg(
				"** ~p ~p non-fatal error in ~p/~p~n"
				"   for the reason ~p:~p~n",
				[?MODULE, self(), send, 2, error, Reason]),
			ok
	catch
		Class:Reason ->
			error_logger:error_msg(
				"** ~p ~p terminating in ~p/~p~n"
				"   for the reason ~p:~p~n** Stacktrace: ~p~n~n",
				[?MODULE, self(), send, 2, Class, Reason, erlang:get_stacktrace()]),
			erlang:error(Reason)
	end.

%% @private
start_announce(initial) ->
	start_announce(crypto:rand_uniform(500, 1500));
start_announce(Service=#service{ttl=TTL}) ->
	ok = stop_announce(Service),
	start_announce(crypto:rand_uniform(TTL * 500, TTL * 900));
start_announce(N) when is_integer(N) ->
	erlang:start_timer(N, self(), announce).

%% @private
stop_announce(#service{aref=undefined}) ->
	ok;
stop_announce(#service{aref=ARef}) when is_reference(ARef) ->
	catch erlang:cancel_timer(ARef),
	ok.

%% @private
start_authorize(initial) ->
	start_authorize(0);
start_authorize(Service=#service{}) ->
	ok = stop_authorize(Service),
	start_authorize(0);
start_authorize(N) when is_integer(N) ->
	erlang:start_timer(N, self(), authorize).

%% @private
stop_authorize(#service{zref=undefined}) ->
	ok;
stop_authorize(#service{zref=ZRef}) when is_reference(ZRef) ->
	catch erlang:cancel_timer(ZRef),
	ok.

%% @private
start_connect(initial) ->
	start_connect(0);
start_connect(Service=#service{backoff=Backoff}) ->
	ok = stop_connect(Service),
	backoff:fire(Backoff);
start_connect(N) when is_integer(N) ->
	erlang:start_timer(N, self(), connect).

%% @private
stop_connect(#service{bref=undefined}) ->
	ok;
stop_connect(#service{bref=BRef}) when is_reference(BRef) ->
	catch erlang:cancel_timer(BRef),
	ok.

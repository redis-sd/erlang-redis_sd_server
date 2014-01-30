%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2014, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  30 Aug 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redis_sd_server_service).
-behaviour(gen_server).

-include("redis_sd_server.hrl").

%% API
-export([start_link/1, enable/1, disable/1, graceful_shutdown/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @private
start_link(Service=?REDIS_SD_SERVICE{name=Name}) ->
	gen_server:start_link({local, Name}, ?MODULE, Service, []).

%% @doc Enable announcing of the service.
enable(Name) ->
	gen_server:cast(Name, enable).

%% @doc Disable announcing of the service.
disable(Name) ->
	gen_server:cast(Name, disable).

%% @doc Gracefully shutdown the service.
graceful_shutdown(Name) ->
	gen_server:call(Name, graceful_shutdown).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init(Service=?REDIS_SD_SERVICE{enabled=Enabled, name=Name, min_wait=MinWait, max_wait=MaxWait}) ->
	case redis_sd_server_dns:refresh(Service) of
		{ok, _Data, Service2=?REDIS_SD_SERVICE{rec=Rec}} ->
			true = redis_sd_server:set_pid(Name, self()),
			true = redis_sd_server:set_enabled(Name, Enabled),
			true = redis_sd_server:set_record(Name, Rec),
			redis_sd_server_event:service_init(Service2),
			case Enabled of
				false ->
					redis_sd_server_event:service_disable(Service2);
				true ->
					redis_sd_server_event:service_enable(Service2)
			end,
			Backoff = backoff:init(timer:seconds(MinWait), timer:seconds(MaxWait), self(), connect),
			BRef = start_connect(initial),
			{ok, Service2?REDIS_SD_SERVICE{backoff=Backoff, bref=BRef}};
		RefreshError ->
			{stop, RefreshError}
	end.

%% @private
handle_call(graceful_shutdown, _From, Service) ->
	{stop, normal, ok, Service};
handle_call(Request, From, Service=?REDIS_SD_SERVICE{name=Name}) ->
	error_logger:warning_msg(
		"** ~p ~p unhandled request from ~p in ~p/~p~n"
		"   Request was: ~p~n",
		[?MODULE, Name, From, handle_call, 3, Request]),
	{reply, ignore, Service}.

%% @private
handle_cast(enable, Service=?REDIS_SD_SERVICE{enabled=false, name=Name, redis_cli=Client}) ->
	true = redis_sd_server:set_enabled(Name, true),
	case Client of
		undefined ->
			{noreply, Service?REDIS_SD_SERVICE{enabled=true}};
		_ ->
			ok = stop_announce(Service),
			announce(Service?REDIS_SD_SERVICE{enabled=true})
	end;
handle_cast(enable, Service=?REDIS_SD_SERVICE{enabled=true}) ->
	{noreply, Service};
handle_cast(disable, Service=?REDIS_SD_SERVICE{enabled=true, name=Name}) ->
	true = redis_sd_server:set_enabled(Name, false),
	{noreply, Service?REDIS_SD_SERVICE{enabled=false}};
handle_cast(disable, Service=?REDIS_SD_SERVICE{enabled=false}) ->
	{noreply, Service};
handle_cast(Request, Service=?REDIS_SD_SERVICE{name=Name}) ->
	error_logger:warning_msg(
		"** ~p ~p unhandled request in ~p/~p~n"
		"   Request was: ~p~n",
		[?MODULE, Name, handle_cast, 2, Request]),
	{noreply, Service}.

%% @private
handle_info({timeout, BRef, connect}, Service=?REDIS_SD_SERVICE{bref=BRef}) ->
	connect(Service?REDIS_SD_SERVICE{bref=undefined});
handle_info({timeout, ARef, announce}, Service=?REDIS_SD_SERVICE{aref=ARef}) ->
	announce(Service?REDIS_SD_SERVICE{aref=undefined});
handle_info({redis_error, Client, {Error, Reason}}, Service=?REDIS_SD_SERVICE{redis_cli=Client, name=Name}) ->
	error_logger:warning_msg(
		"** ~p ~p received redis_error in ~p/~p~n"
		"   for the reason ~p:~p~n",
		[?MODULE, Name, handle_info, 3, Error, Reason]),
	{noreply, Service};
handle_info({redis_closed, Client}, Service=?REDIS_SD_SERVICE{redis_cli=Client}) ->
	ok = stop_announce(Service),
	ok = hierdis_async:close(Client),
	connect(Service?REDIS_SD_SERVICE{redis_cli=undefined, aref=undefined});
handle_info(Info, Service=?REDIS_SD_SERVICE{name=Name}) ->
	error_logger:error_msg(
		"** ~p ~p unhandled info in ~p/~p~n"
		"   Info was: ~p~n",
		[?MODULE, Name, handle_info, 3, Info]),
	{noreply, Service}.

%% @private
terminate(normal, Service=?REDIS_SD_SERVICE{redis_cli=Client}) when Client =/= undefined ->
	catch announce(Service?REDIS_SD_SERVICE{ttl=0}),
	catch hierdis_async:close(Client),
	redis_sd_server_event:service_terminate(normal, Service?REDIS_SD_SERVICE{redis_cli=undefined, ttl=0}),
	ok;
terminate(Reason, Service=?REDIS_SD_SERVICE{redis_cli=Client}) ->
	case Client of
		undefined ->
			ok;
		_ ->
			catch hierdis_async:close(Client)
	end,
	redis_sd_server_event:service_terminate(Reason, Service?REDIS_SD_SERVICE{redis_cli=undefined}),
	ok.

%% @private
code_change(_OldVsn, Service, _Extra) ->
	{ok, Service}.

%%%===================================================================
%%% States
%%%===================================================================

%% @private
connect(Service=?REDIS_SD_SERVICE{redis_opts={Transport, Args}, backoff=Backoff}) ->
	ConnectFun = case Transport of
		tcp ->
			connect;
		unix ->
			connect_unix
	end,
	case erlang:apply(hierdis_async, ConnectFun, Args) of
		{ok, Client} ->
			authorize(Service?REDIS_SD_SERVICE{redis_cli=Client});
		{error, _ConnectError} ->
			BRef = start_connect(Service),
			{_Delay, Backoff2} = backoff:fail(Backoff),
			{noreply, Service?REDIS_SD_SERVICE{backoff=Backoff2, bref=BRef}}
	end.

%% @private
authorize(Service=?REDIS_SD_SERVICE{redis_auth=undefined, backoff=Backoff}) ->
	redis_sd_server_event:service_connect(Service),
	ARef = start_announce(initial),
	{_Start, Backoff2} = backoff:succeed(Backoff),
	{noreply, Service?REDIS_SD_SERVICE{aref=ARef, backoff=Backoff2}};
authorize(Service=?REDIS_SD_SERVICE{redis_auth=Password, backoff=Backoff}) when Password =/= undefined ->
	case redis_auth(Password, Service) of
		ok ->
			redis_sd_server_event:service_connect(Service),
			ARef = start_announce(initial),
			{_Start, Backoff2} = backoff:succeed(Backoff),
			{noreply, Service?REDIS_SD_SERVICE{aref=ARef, backoff=Backoff2}};
		{error, AuthError} ->
			error_logger:warning_msg("[~p] Redis auth error: ~p", [?MODULE, AuthError]),
			BRef = start_connect(Service),
			{_Delay, Backoff2} = backoff:fail(Backoff),
			{noreply, Service?REDIS_SD_SERVICE{backoff=Backoff2, bref=BRef}}
	end.

%% @private
announce(Service) ->
	try
		case redis_sd_server_dns:refresh(Service) of
			{ok, Data, Service2=?REDIS_SD_SERVICE{name=Name, rec=Rec}} ->
				true = redis_sd_server:set_record(Name, Rec),
				case Service?REDIS_SD_SERVICE.enabled of
					false ->
						ok;
					true ->
						ok = redis_announce(Data, Service2)
				end,
				ARef = start_announce(Service2),
				{noreply, Service2?REDIS_SD_SERVICE{aref=ARef}};
			RefreshError ->
				erlang:error(RefreshError)
		end
	catch
		Class:Reason ->
			error_logger:warning_msg(
				"** ~p ~p terminating in ~p/~p~n"
				"   for the reason ~p:~p~n"
				"** Stacktrace: ~p~n~n",
				[?MODULE, self(), announce, 2, Class, Reason, erlang:get_stacktrace()]),
			ok = stop_announce(Service),
			{stop, {Class, Reason}, Service?REDIS_SD_SERVICE{aref=undefined}}
	end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
redis_auth(Password, ?REDIS_SD_SERVICE{redis_cli=Client, cmd_auth=AUTH}) ->
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
redis_announce(Data, Service=?REDIS_SD_SERVICE{key=KEY, rec=?REDIS_SD_DNS{ttl=TTL}, redis_cli=Client, redis_ns=Namespace, cmd_del=DEL, cmd_publish=PUBLISH, cmd_setex=SETEX}) ->
	redis_sd_server_event:service_announce(Data, Service),
	Channel = [Namespace, "KEY:", KEY],
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
				[?MODULE, self(), redis_announce, 2, error, Reason]),
			ok
	catch
		Class:Reason ->
			error_logger:error_msg(
				"** ~p ~p terminating in ~p/~p~n"
				"   for the reason ~p:~p~n** Stacktrace: ~p~n~n",
				[?MODULE, self(), redis_announce, 2, Class, Reason, erlang:get_stacktrace()]),
			erlang:error(Reason)
	end.

%% @private
start_connect(initial) ->
	start_connect(0);
start_connect(Service=?REDIS_SD_SERVICE{backoff=Backoff}) ->
	ok = stop_connect(Service),
	backoff:fire(Backoff);
start_connect(N) when is_integer(N) ->
	erlang:start_timer(N, self(), connect).

%% @private
stop_connect(?REDIS_SD_SERVICE{bref=undefined}) ->
	ok;
stop_connect(?REDIS_SD_SERVICE{bref=BRef}) when is_reference(BRef) ->
	catch erlang:cancel_timer(BRef),
	ok.

%% @private
start_announce(initial) ->
	start_announce(crypto:rand_uniform(500, 1500));
start_announce(Service=?REDIS_SD_SERVICE{rec=?REDIS_SD_DNS{ttl=0}}) ->
	ok = stop_announce(Service),
	start_announce(0);
start_announce(Service=?REDIS_SD_SERVICE{rec=?REDIS_SD_DNS{ttl=TTL}}) ->
	ok = stop_announce(Service),
	start_announce(crypto:rand_uniform(TTL * 500, TTL * 900));
start_announce(N) when is_integer(N) ->
	erlang:start_timer(N, self(), announce).

%% @private
stop_announce(?REDIS_SD_SERVICE{aref=undefined}) ->
	ok;
stop_announce(?REDIS_SD_SERVICE{aref=ARef}) when is_reference(ARef) ->
	catch erlang:cancel_timer(ARef),
	ok.

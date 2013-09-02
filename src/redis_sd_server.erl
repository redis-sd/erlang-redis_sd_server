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
-module(redis_sd_server).
-behaviour(gen_server).

-include("redis_sd_server.hrl").

-define(SERVER, ?MODULE).

%% API
-export([manual_start/0, new_service/1, rm_service/1, delete_service/1, start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Manually start redis_sd_server and all dependencies.
-spec manual_start() -> ok.
manual_start() ->
	redis_sd:require([
		hierdis,
		redis_sd_server
	]).

new_service(ServiceConfig) ->
	redis_sd_server_sup:new_service(ServiceConfig).

%% @doc Gracefully terminate the named service.
rm_service(ServiceName) ->
	redis_sd_server_sup:rm_service(ServiceName).

%% @doc Forcefully terminate the named service.
delete_service(ServiceName) ->
	redis_sd_server_sup:delete_service(ServiceName).

%% @private
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%% @private
init([]) ->
	ok = redis_sd_server_event:add_handler(redis_sd_event_handler, self()),
	{ok, undefined}.

%% @private
handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

%% @private
handle_cast(_Request, State) ->
	{noreply, State}.

%% @private
handle_info({'$redis_sd', {service, terminate, normal, #service{name=Name}}}, State) ->
	ok = redis_sd_server:delete_service(Name),
	{noreply, State};
handle_info({'$redis_sd', _Event}, State) ->
	{noreply, State};
handle_info(Info, State) ->
	error_logger:error_msg(
		"** ~p ~p unhandled info in ~p/~p~n"
		"   Info was: ~p~n",
		[?MODULE, self(), handle_info, 2, Info]),
	{noreply, State}.

%% @private
terminate(_Reason, _State) ->
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

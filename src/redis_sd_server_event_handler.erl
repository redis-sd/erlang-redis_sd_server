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
-module(redis_sd_server_event_handler).
-behaviour(gen_event).

-include("redis_sd_server.hrl").

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
		 terminate/2, code_change/3]).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%% @private
init(Pid) ->
	{ok, Pid}.

%% @private
handle_event({service, init, Service}, Pid) ->
	catch Pid ! {'$redis_sd_service_init', Service},
	{ok, Pid};
handle_event({service, connect, Service}, Pid) ->
	catch Pid ! {'$redis_sd_service_connect', Service},
	{ok, Pid};
handle_event({service, announce, Data, Service}, Pid) ->
	catch Pid ! {'$redis_sd_service_announce', Data, Service},
	{ok, Pid};
handle_event({service, terminate, Reason, Service}, Pid) ->
	catch Pid ! {'$redis_sd_service_terminate', Reason, Service},
	{ok, Pid};
handle_event(_Event, Pid) ->
	{ok, Pid}.

%% @private
handle_call(_Request, Pid) ->
	{ok, ok, Pid}.

%% @private
handle_info({'EXIT', _Parent, shutdown}, _Pid) ->
	remove_handler;
handle_info(_Info, Pid) ->
	{ok, Pid}.

%% @private
terminate(_Reason, _Pid) ->
	ok.

%% @private
code_change(_OldVsn, Pid, _Extra) ->
	{ok, Pid}.

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
-module(redis_sd_server).
-behaviour(gen_server).

-include("redis_sd_server.hrl").

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).

%% API
-export([manual_start/0, start_link/0]).
-export([enable/0, enable/1, disable/0, disable/1, list/0, list_services/0]).
-export([new_service/1, rm_service/1, delete_service/1, get_record/1,
	set_enabled/2, set_pid/2, set_record/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-type monitors() :: [{{reference(), pid()}, any()}].
-record(state, {
	monitors = [] :: monitors()
}).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Manually start redis_sd_server and all dependencies.
-spec manual_start() -> ok.
manual_start() ->
	redis_sd:require([
		backoff,
		hierdis,
		redis_sd_spec,
		redis_sd_server
	]).

%% @private
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Enables all services.
enable() ->
	enable([Pid || {_Name, Pid} <- list()]).

%% @doc Enables the list of service names or pids.
enable(Services) when is_list(Services) ->
	_ = [redis_sd_server_service:enable(S) || S <- Services],
	ok.

%% @doc Disables all services.
disable() ->
	disable([Pid || {_Name, Pid} <- list()]).

%% @doc Disables the list of service names or pids.
disable(Services) when is_list(Services) ->
	_ = [redis_sd_server_service:disable(S) || S <- Services],
	ok.

%% @doc List all service names and their pids.
list() ->
	ets:select(?TAB, [{{{pid, '$1'}, '$2'}, [], [{{'$1', '$2'}}]}]).

%% @doc List all services in the format of: {Name, {Enabled, Pid, Record}}
list_services() ->
	FoldFun = fun
		({{enabled, Name}, Enabled}, Acc) ->
			UpdateFun = fun({_, Pid, Record}) ->
				{Enabled, Pid, Record}
			end,
			orddict:update(Name, UpdateFun, {Enabled, undefined, undefined}, Acc);
		({{pid, Name}, Pid}, Acc) ->
			UpdateFun = fun({Enabled, _, Record}) ->
				{Enabled, Pid, Record}
			end,
			orddict:update(Name, UpdateFun, {undefined, Pid, undefined}, Acc);
		({{record, Name}, Record}, Acc) ->
			UpdateFun = fun({Enabled, Pid, _}) ->
				{Enabled, Pid, Record}
			end,
			orddict:update(Name, UpdateFun, {undefined, undefined, Record}, Acc)
	end,
	ets:foldl(FoldFun, orddict:new(), ?TAB).

new_service(ServiceConfig) ->
	redis_sd_server_sup:new_service(ServiceConfig).

%% @doc Gracefully terminate the named service.
rm_service(ServiceName) ->
	redis_sd_server_sup:rm_service(ServiceName).

%% @doc Forcefully terminate the named service.
delete_service(ServiceName) ->
	redis_sd_server_sup:delete_service(ServiceName).

get_record(ServiceName) ->
	case ets:lookup(?TAB, {record, ServiceName}) of
		[{{record, ServiceName}, Record}] ->
			{ok, Record};
		_ ->
			{error, service_not_found}
	end.

%% @private
set_enabled(ServiceName, Enabled) ->
	case is_service_owner(ServiceName) of
		false ->
			false;
		true ->
			gen_server:call(?SERVER, {set_enabled, ServiceName, Enabled})
	end.

%% @private
set_pid(ServiceName, Pid) ->
	gen_server:call(?SERVER, {set_pid, ServiceName, Pid}).

%% @private
set_record(ServiceName, Record) ->
	case is_service_owner(ServiceName) of
		false ->
			false;
		true ->
			gen_server:call(?SERVER, {set_record, ServiceName, Record})
	end.

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%% @private
init([]) ->
	Monitors = [{{erlang:monitor(process, Pid), Pid}, Ref} || [Ref, Pid] <- ets:match(?TAB, {{pid, '$1'}, '$2'})],
	ok = redis_sd_server_event:add_handler(redis_sd_event_handler, self()),
	{ok, #state{monitors=Monitors}}.

%% @private
handle_call({set_enabled, Ref, Enabled}, _From, State) ->
	Reply = ets:insert(?TAB, {{enabled, Ref}, Enabled}),
	{reply, Reply, State};
handle_call({set_pid, Ref, Pid}, _From, State=#state{monitors=Monitors}) ->
	case ets:insert_new(?TAB, {{pid, Ref}, Pid}) of
		true ->
			MonitorRef = erlang:monitor(process, Pid),
			{reply, true, State#state{monitors=[{{MonitorRef, Pid}, Ref} | Monitors]}};
		false ->
			{reply, false, State}
	end;
handle_call({set_record, Ref, Record}, _From, State) ->
	Reply = ets:insert(?TAB, {{record, Ref}, Record}),
	{reply, Reply, State};
handle_call(_Request, _From, State) ->
	{reply, ignore, State}.

%% @private
handle_cast(_Request, State) ->
	{noreply, State}.

%% @private
handle_info({'$redis_sd', {service, terminate, normal, ?REDIS_SD_SERVICE{name=Name}}}, State) ->
	ok = redis_sd_server:delete_service(Name),
	{noreply, State};
handle_info({'$redis_sd', _Event}, State) ->
	{noreply, State};
handle_info({'DOWN', MonitorRef, process, Pid, _Reason}, State=#state{monitors=Monitors}) ->
	case lists:keytake({MonitorRef, Pid}, 1, Monitors) of
		{value, {{MonitorRef, Pid}, Ref}, Monitors2} ->
			true = ets:delete(?TAB, {pid, Ref}),
			true = ets:delete(?TAB, {enabled, Ref}),
			true = ets:delete(?TAB, {record, Ref}),
			{noreply, State#state{monitors=Monitors2}};
		false ->
			{noreply, State}
	end;
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

%% @private
is_service_owner(ServiceName) ->
	Self = self(),
	try ets:lookup_element(?TAB, {pid, ServiceName}, 2) of
		Self ->
			true;
		_ ->
			false
	catch
		_:_ ->
			false
	end.

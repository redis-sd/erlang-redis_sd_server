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
-module(redis_sd_server_dns).

-include("redis_sd_server.hrl").

%% API
-export([refresh/1, resolve/1, resolve/2, record/1, data/1]).

%% Internal
-export([val/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Refreshes
refresh(Service=#service{}) ->
	case resolve(Service#service{obj=undefined}) of
		{ok, Resolved} ->
			data(Resolved);
		ResolveError ->
			ResolveError
	end.

%% @doc Resolves any dynamic function values in the Service.
resolve(Service=#service{}) ->
	Keys = [domain, type, service, instance, ttl, priority, weight,
		port, target, txtdata, keys],
	resolve(Keys, Service).

%% @doc Resolves a specific key in the Service.
resolve(Keys, Service=#service{}) when is_list(Keys) ->
	s(Keys, Service);
resolve(Key, Service=#service{}) when is_atom(Key) ->
	resolve([Key], Service).

%% @doc Creates a new dns_rec record based on the Service.
record(Service=#service{}) ->
	Record = inet_dns:make_msg([
		{header, header()},
		{anlist, anlist(Service)},
		{arlist, arlist(Service)}
	]),
	{ok, Record, Service}.

%% @doc Creates and encodes a new dns_rec record based on the Service.
data(Service=#service{}) ->
	case record(Service) of
		{ok, Record, Service2} ->
			Data = inet_dns:encode(Record),
			{ok, Data, Service2};
		RecordError ->
			{error, RecordError}
	end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
s([], Service) ->
	{ok, Service};
s([Key | Keys], Service) ->
	case setval(Key, Service) of
		Service2=#service{} ->
			s(Keys, Service2);
		Error ->
			Error
	end.

%% @private
setval(Key, S=#service{obj=undefined}) ->
	setval(Key, S#service{obj=#dns_sd{}});
setval(domain, S=#service{domain=V, obj=Obj}) ->
	V2 = redis_sd:any_to_binary(val(Obj, V)),
	D = redis_sd_ns:split(V2),
	case lists:all(fun(B) -> redis_sd_ns:is_label(B) end, D) of
		true ->
			S#service{obj=Obj#dns_sd{domain=V2}};
		false ->
			{error, {invalid_label, {domain, V2}}}
	end;
setval(type, S=#service{type=V, obj=Obj}) ->
	V2 = redis_sd:any_to_binary(val(Obj, V)),
	case redis_sd_ns:is_label(V2) of
		true ->
			S#service{obj=Obj#dns_sd{type=V2}};
		false ->
			{error, {invalid_label, {type, V2}}}
	end;
setval(service, S=#service{service=V, obj=Obj}) ->
	V2 = redis_sd:any_to_binary(val(Obj, V)),
	case redis_sd_ns:is_label(V2) of
		true ->
			S#service{obj=Obj#dns_sd{service=V2}};
		false ->
			{error, {invalid_label, {service, V2}}}
	end;
setval(instance, S=#service{instance=V, obj=Obj}) ->
	V2 = redis_sd:any_to_binary(val(Obj, V)),
	S#service{obj=Obj#dns_sd{instance=V2}};
setval(ttl, S=#service{ttl=V, obj=Obj}) ->
	V2 = val(Obj, V),
	S#service{obj=Obj#dns_sd{ttl=V2}};
setval(priority, S=#service{priority=V, obj=Obj}) ->
	V2 = val(Obj, V),
	S#service{obj=Obj#dns_sd{priority=V2}};
setval(weight, S=#service{weight=V, obj=Obj}) ->
	V2 = val(Obj, V),
	S#service{obj=Obj#dns_sd{weight=V2}};
setval(port, S=#service{port=V, obj=Obj}) ->
	V2 = val(Obj, V),
	S#service{obj=Obj#dns_sd{port=V2}};
setval(target, S=#service{target=V, obj=Obj}) ->
	V2 = redis_sd:any_to_binary(val(Obj, V)),
	S#service{obj=Obj#dns_sd{target=V2}};
setval(txtdata, S=#service{txtdata=V, obj=Obj}) ->
	V2 = val(Obj, V),
	S#service{obj=Obj#dns_sd{txtdata=V2}};
setval(keys, S=#service{obj=#dns_sd{domain=Domain, type=Type, service=Service, instance=Instance}}) ->
	Keys = redis_sd:labels_to_keys({Domain, Type, Service, Instance}),
	PTR = proplists:get_value(ptr, Keys),
	SRV = proplists:get_value(srv, Keys),
	KEY = proplists:get_value(key, Keys),
	S#service{ptr=PTR, srv=SRV, key=KEY}.

%% @private
val(Obj, {Module, Function, Arguments}) when is_atom(Module) andalso is_atom(Function) andalso is_list(Arguments) ->
	erlang:apply(Module, Function, [Obj | Arguments]);
val(Obj, {Function, Arguments}) when is_function(Function) andalso is_list(Arguments) ->
	erlang:apply(Function, [Obj | Arguments]);
val(Obj, Function) when is_function(Function, 1) ->
	Function(Obj);
val(_Obj, Function) when is_function(Function, 0) ->
	Function();
val(_Obj, Val) ->
	Val.

%% @private
header() ->
	inet_dns:make_header([
		{id, 0},
		{qr, true},
		{opcode, 'query'},
		{aa, true},
		{tc, false},
		{rd, false},
		{ra, false},
		{pr, false},
		{rcode, 0}
	]).

%% @private
anlist(Service) ->
	[answer(Service)].

%% @private
arlist(Service) ->
	[service(Service), text(Service)].

%% @private
answer(#service{ptr=PTR, srv=SRV, obj=#dns_sd{ttl=TTL}}) ->
	inet_dns:make_rr([
		{type, ptr},
		{domain, redis_sd:any_to_string(PTR)},
		{class, in},
		{ttl, TTL},
		{data, redis_sd:any_to_string(SRV)}
	]).

%% @private
service(#service{srv=SRV, obj=#dns_sd{ttl=TTL, priority=Priority, weight=Weight, port=Port, target=Target}}) ->
	inet_dns:make_rr([
		{type, srv},
		{domain, redis_sd:any_to_string(SRV)},
		{class, in},
		{ttl, TTL},
		{data, {Priority, Weight, Port, redis_sd:any_to_string(Target)}}
	]).

%% @private
text(#service{srv=SRV, obj=#dns_sd{ttl=TTL, txtdata=TXTData}}) ->
	inet_dns:make_rr([
		{type, txt},
		{domain, redis_sd:any_to_string(SRV)},
		{class, in},
		{ttl, TTL},
		{data, text_data(TXTData, [])}
	]).

%% @private
text_data([], Acc) ->
	lists:reverse(Acc);
text_data([{Key, Val} | TXTData], Acc) ->
	KeyVal = redis_sd:any_to_string([text_data_encode(Key), $=, text_data_encode(Val)]),
	text_data(TXTData, [KeyVal | Acc]).

%% @private
text_data_encode(Data) when is_function(Data) ->
	text_data_encode(Data());
text_data_encode(Data) ->
	redis_sd:urlencode(Data).

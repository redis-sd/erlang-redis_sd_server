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

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Refreshes
refresh(Service=#service{}) ->
	case resolve(Service) of
		{ok, Resolved} ->
			data(Resolved);
		ResolveError ->
			{error, ResolveError}
	end.

%% @doc Resolves any dynamic function values in the Service.
resolve(Service=#service{}) ->
	Keys = [service, type, domain, hostname, ttl, host, port, txtdata,
		hostkey, servkey, typekey],
	s(Keys, Service).

%% @doc Resolves a specific key in the Service.
resolve(Keys, Service=#service{}) when is_list(Keys) ->
	s(Keys, Service);
resolve(Key, Service=#service{}) when is_atom(Key) ->
	{ok, setval(Key, Service)}.

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
	s(Keys, setval(Key, Service)).

%% @private
setval(service, S=#service{service=V}) ->
	S#service{service=val(V)};
setval(type, S=#service{type=V}) ->
	S#service{type=val(V)};
setval(domain, S=#service{domain=V}) ->
	S#service{domain=val(V)};
setval(hostname, S=#service{hostname=undefined}) ->
	{ok, H} = inet:gethostname(),
	S#service{hostname=H};
setval(hostname, S=#service{hostname=V}) ->
	S#service{hostname=val(V)};
setval(ttl, S=#service{ttl=V}) ->
	S#service{ttl=val(V)};
setval(host, S=#service{host=undefined}) ->
	S2 = setval(hostname, S),
	S2#service{host=S2#service.hostname};
setval(host, S=#service{host=V}) ->
	S#service{host=val(V)};
setval(port, S=#service{port=V}) ->
	S#service{port=val(V)};
setval(txtdata, S=#service{txtdata=V}) ->
	S#service{txtdata=val(V)};
setval(hostkey, S=#service{hostkey=undefined}) ->
	{ok, S2} = s([host, domain], S),
	HostKey = iolist_to_binary([
		redis_sd:urlencode(S2#service.host),
		$., redis_sd:urlencode(S2#service.domain)
	]),
	S2#service{hostkey=HostKey};
setval(hostkey, S=#service{hostkey=V}) ->
	S#service{hostkey=val(V)};
setval(servkey, S=#service{servkey=undefined}) ->
	{ok, S2} = s([hostname, service, type, domain], S),
	ServKey = iolist_to_binary([
		redis_sd:urlencode(S2#service.hostname),
		$., $_, redis_sd:urlencode(S2#service.service),
		$., $_, redis_sd:urlencode(S2#service.type),
		$., redis_sd:urlencode(S2#service.domain)
	]),
	S2#service{servkey=ServKey};
setval(servkey, S=#service{servkey=V}) ->
	S#service{servkey=val(V)};
setval(typekey, S=#service{typekey=undefined}) ->
	{ok, S2} = s([service, type, domain], S),
	TypeKey = iolist_to_binary([
		$_, redis_sd:urlencode(S2#service.service),
		$., $_, redis_sd:urlencode(S2#service.type),
		$., redis_sd:urlencode(S2#service.domain)
	]),
	S2#service{typekey=TypeKey};
setval(typekey, S=#service{typekey=V}) ->
	S#service{typekey=val(V)}.

%% @private
val({Module, Function, Arguments}) when is_atom(Module) andalso is_atom(Function) andalso is_list(Arguments) ->
	erlang:apply(Module, Function, Arguments);
val({Function, Arguments}) when is_function(Function) andalso is_list(Arguments) ->
	erlang:apply(Function, Arguments);
val(Function) when is_function(Function, 0) ->
	Function();
val(Val) ->
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
answer(#service{servkey=ServKey, typekey=TypeKey, ttl=TTL}) ->
	inet_dns:make_rr([
		{type, ptr},
		{domain, redis_sd:any_to_string(TypeKey)},
		{class, in},
		{ttl, TTL},
		{data, redis_sd:any_to_string(ServKey)}
	]).

%% @private
service(Service=#service{servkey=ServKey, hostkey=HostKey, ttl=TTL}) ->
	Port = case Service#service.port of
		undefined ->
			0;
		_ ->
			Service#service.port
	end,
	inet_dns:make_rr([
		{type, srv},
		{domain, redis_sd:any_to_string(ServKey)},
		{class, in},
		{ttl, TTL},
		{data, {0, 0, Port, redis_sd:any_to_string(HostKey)}}
	]).

%% @private
text(#service{servkey=ServKey, txtdata=TXTData, ttl=TTL}) ->
	inet_dns:make_rr([
		{type, txt},
		{domain, redis_sd:any_to_string(ServKey)},
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

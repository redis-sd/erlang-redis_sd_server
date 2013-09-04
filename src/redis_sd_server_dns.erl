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
			ResolveError
	end.

%% @doc Resolves any dynamic function values in the Service.
resolve(Service=#service{}) ->
	Keys = [service, type, domain, hostname, instance, ttl, host, port,
		txtdata, hostkey, instkey, servkey, typekey],
	s(Keys, Service).

%% @doc Resolves a specific key in the Service.
resolve(Keys, Service=#service{}) when is_list(Keys) ->
	s(Keys, Service);
resolve(Key, Service=#service{}) when is_atom(Key) ->
	s([Key], Service).

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
setval(service, S=#service{service=V}) ->
	V2 = val(V),
	case redis_sd:is_label(V2) of
		true ->
			S#service{service=V2};
		false ->
			{error, {invalid_label, {service, V2}}}
	end;
setval(type, S=#service{type=V}) ->
	V2 = val(V),
	case redis_sd:is_label(V2) of
		true ->
			S#service{type=V2};
		false ->
			{error, {invalid_label, {type, V2}}}
	end;
setval(domain, S=#service{domain=V}) ->
	V2 = val(V),
	D = redis_sd:nssplit(V2),
	case lists:all(fun(B) -> redis_sd:is_label(binary_to_list(B)) end, D) of
		true ->
			S#service{domain=D};
		false ->
			{error, {invalid_label, {domain, V2}}}
	end;
setval(hostname, S=#service{hostname=undefined}) ->
	{ok, H} = inet:gethostname(),
	S#service{hostname=H};
setval(hostname, S=#service{hostname=V}) ->
	S#service{hostname=val(V)};
setval(instance, S=#service{instance=undefined}) ->
	I = integer_to_list(erlang:phash2(node())),
	S#service{instance=I};
setval(instance, S=#service{instance=V}) ->
	S#service{instance=val(V)};
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
	case s([host, domain], S) of
		{ok, S2} ->
			HostKey = iolist_to_binary([
				S2#service.host,
				$., S2#service.domain
			]),
			S2#service{hostkey=HostKey};
		Error ->
			Error
	end;
setval(hostkey, S=#service{hostkey=V}) ->
	S#service{hostkey=val(V)};
setval(instkey, S=#service{instkey=undefined}) ->
	case s([instance, hostname, service, type, domain], S) of
		{ok, S2} ->
			InstKey = iolist_to_binary([
				redis_sd:urlencode(S2#service.instance),
				$., S2#service.hostname,
				$., $_, S2#service.service,
				$., $_, S2#service.type,
				$., S2#service.domain
			]),
			S2#service{instkey=InstKey};
		Error ->
			Error
	end;
setval(instkey, S=#service{instkey=V}) ->
	S#service{instkey=val(V)};
setval(servkey, S=#service{servkey=undefined}) ->
	case s([hostname, service, type, domain], S) of
		{ok, S2} ->
			ServKey = iolist_to_binary([
				S2#service.hostname,
				$., $_, S2#service.service,
				$., $_, S2#service.type,
				$., S2#service.domain
			]),
			S2#service{servkey=ServKey};
		Error ->
			Error
	end;
setval(servkey, S=#service{servkey=V}) ->
	S#service{servkey=val(V)};
setval(typekey, S=#service{typekey=undefined}) ->
	case s([service, type, domain], S) of
		{ok, S2} ->
			TypeKey = iolist_to_binary([
				$_, S2#service.service,
				$., $_, S2#service.type,
				$., S2#service.domain
			]),
			S2#service{typekey=TypeKey};
		Error ->
			Error
	end;
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
answer(#service{instkey=InstKey, typekey=TypeKey, ttl=TTL}) ->
	inet_dns:make_rr([
		{type, ptr},
		{domain, redis_sd:any_to_string(TypeKey)},
		{class, in},
		{ttl, TTL},
		{data, redis_sd:any_to_string(InstKey)}
	]).

%% @private
service(Service=#service{instkey=InstKey, hostkey=HostKey, ttl=TTL}) ->
	Port = case Service#service.port of
		undefined ->
			0;
		_ ->
			Service#service.port
	end,
	inet_dns:make_rr([
		{type, srv},
		{domain, redis_sd:any_to_string(InstKey)},
		{class, in},
		{ttl, TTL},
		{data, {0, 0, Port, redis_sd:any_to_string(HostKey)}}
	]).

%% @private
text(#service{instkey=InstKey, txtdata=TXTData, ttl=TTL}) ->
	inet_dns:make_rr([
		{type, txt},
		{domain, redis_sd:any_to_string(InstKey)},
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

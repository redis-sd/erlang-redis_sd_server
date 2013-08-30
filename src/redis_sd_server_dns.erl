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
-export([get/2, host_key/1, serv_key/1, type_key/1, hostname/1,
	record/1, data/1, refresh/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Update and return the list of keys of the Service.
get([], Service) ->
	{ok, [], Service};
get(List, Service) ->
	g(List, Service, []).

%% @doc Update and return the HostKey of the Service.
%% For example: <<"host.domain">>
host_key(Service=#service{domain=Domain, hostname=Host}) ->
	HostKey = iolist_to_binary([
		redis_sd:urlencode(Host),
		$., redis_sd:urlencode(Domain)
	]),
	Service2 = Service#service{hostkey=HostKey},
	{ok, HostKey, Service2}.

%% @doc Update and return the ServKey of the Service.
%% For example: <<"host._service._type.domain">>
serv_key(Service=#service{service=ServiceName, type=Type, domain=Domain, hostname=Host}) ->
	ServKey = iolist_to_binary([
		redis_sd:urlencode(Host),
		$., $_, redis_sd:urlencode(ServiceName),
		$., $_, redis_sd:urlencode(Type),
		$., redis_sd:urlencode(Domain)
	]),
	Service2 = Service#service{servkey=ServKey},
	{ok, ServKey, Service2}.

%% @doc Update and return the TypeKey of the Service.
%% For example: <<"_service._type.domain">>
type_key(Service=#service{service=ServiceName, type=Type, domain=Domain}) ->
	TypeKey = iolist_to_binary([
		$_, redis_sd:urlencode(ServiceName),
		$., $_, redis_sd:urlencode(Type),
		$., redis_sd:urlencode(Domain)
	]),
	Service2 = Service#service{typekey=TypeKey},
	{ok, TypeKey, Service2}.

%% @doc Update and return the Hostname of the Service.
hostname(Service=#service{host=undefined}) ->
	{ok, Hostname} = inet:gethostname(),
	Service2 = Service#service{hostname=Hostname},
	{ok, Hostname, Service2};
hostname(Service=#service{host=Hostname}) ->
	{ok, Hostname, Service}.

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
		{ok, Record, Service} ->
			Data = inet_dns:encode(Record),
			{ok, Data, Service};
		Error ->
			Error
	end.

%% @doc Refreshes
refresh(Service=#service{}) ->
	case get([hostname, host_key, serv_key, type_key, data], Service) of
		{ok, [_Hostname, _HostKey, _ServKey, _TypeKey, Data], Service2} when is_binary(Data) ->
			{ok, Data, Service2};
		Error ->
			Error
	end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
g([], Service, Acc) ->
	{ok, lists:reverse(Acc), Service};
g([Key | Keys], Service, Acc) when Key =/= refresh ->
	case ?MODULE:Key(Service) of
		{ok, Val, Service2} ->
			g(Keys, Service2, [Val | Acc]);
		Error ->
			Error
	end.

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
	KeyVal = redis_sd:any_to_string([redis_sd:urlencode(Key), $=, redis_sd:urlencode(Val)]),
	text_data(TXTData, [KeyVal | Acc]).

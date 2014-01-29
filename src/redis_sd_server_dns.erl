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
-module(redis_sd_server_dns).

-include("redis_sd_server.hrl").

%% API
-export([refresh/1, resolve/1, resolve/2]).

%% Internal
-export([val/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Resolves the redis_sd_dns again and also returns the binary form.
refresh(Service=?REDIS_SD_SERVICE{}) ->
	case resolve(Service?REDIS_SD_SERVICE{rec=undefined}) of
		{ok, Resolved=?REDIS_SD_SERVICE{rec=Rec}} ->
			{ok, redis_sd_dns:to_binary(Rec), Resolved};
		ResolveError ->
			ResolveError
	end.

%% @doc Resolves any dynamic function values in the Service.
resolve(Service=?REDIS_SD_SERVICE{}) ->
	Keys = [domain, type, service, instance, ttl, priority, weight,
		port, target, txtdata, keys],
	resolve(Keys, Service).

%% @doc Resolves a specific key in the Service.
resolve(Keys, Service=?REDIS_SD_SERVICE{}) when is_list(Keys) ->
	s(Keys, Service);
resolve(Key, Service=?REDIS_SD_SERVICE{}) when is_atom(Key) ->
	resolve([Key], Service).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
s([], Service) ->
	{ok, Service};
s([Key | Keys], Service) ->
	case setval(Key, Service) of
		Service2=?REDIS_SD_SERVICE{} ->
			s(Keys, Service2);
		Error ->
			Error
	end.

%% @private
setval(Key, S=?REDIS_SD_SERVICE{rec=undefined}) ->
	setval(Key, S?REDIS_SD_SERVICE{rec=?REDIS_SD_DNS{}});
setval(domain, S=?REDIS_SD_SERVICE{domain=V, rec=Rec}) ->
	V2 = redis_sd:any_to_binary(val(Rec, V)),
	D = redis_sd_ns:split(V2),
	case lists:all(fun(B) -> redis_sd_ns:is_label(B) end, D) of
		true ->
			S?REDIS_SD_SERVICE{rec=Rec?REDIS_SD_DNS{domain=V2}};
		false ->
			{error, {invalid_label, {domain, V2}}}
	end;
setval(type, S=?REDIS_SD_SERVICE{type=V, rec=Rec}) ->
	V2 = redis_sd:any_to_binary(val(Rec, V)),
	case redis_sd_ns:is_label(V2) of
		true ->
			S?REDIS_SD_SERVICE{rec=Rec?REDIS_SD_DNS{type=V2}};
		false ->
			{error, {invalid_label, {type, V2}}}
	end;
setval(service, S=?REDIS_SD_SERVICE{service=V, rec=Rec}) ->
	V2 = redis_sd:any_to_binary(val(Rec, V)),
	case redis_sd_ns:is_label(V2) of
		true ->
			S?REDIS_SD_SERVICE{rec=Rec?REDIS_SD_DNS{service=V2}};
		false ->
			{error, {invalid_label, {service, V2}}}
	end;
setval(instance, S=?REDIS_SD_SERVICE{instance=V, rec=Rec}) ->
	V2 = redis_sd:any_to_binary(val(rec, V)),
	S?REDIS_SD_SERVICE{rec=Rec?REDIS_SD_DNS{instance=V2}};
setval(ttl, S=?REDIS_SD_SERVICE{ttl=V, rec=Rec}) ->
	V2 = val(Rec, V),
	S?REDIS_SD_SERVICE{rec=Rec?REDIS_SD_DNS{ttl=V2}};
setval(priority, S=?REDIS_SD_SERVICE{priority=V, rec=Rec}) ->
	V2 = val(Rec, V),
	S?REDIS_SD_SERVICE{rec=Rec?REDIS_SD_DNS{priority=V2}};
setval(weight, S=?REDIS_SD_SERVICE{weight=V, rec=Rec}) ->
	V2 = val(Rec, V),
	S?REDIS_SD_SERVICE{rec=Rec?REDIS_SD_DNS{weight=V2}};
setval(port, S=?REDIS_SD_SERVICE{port=V, rec=Rec}) ->
	V2 = val(Rec, V),
	S?REDIS_SD_SERVICE{rec=Rec?REDIS_SD_DNS{port=V2}};
setval(target, S=?REDIS_SD_SERVICE{target=V, rec=Rec}) ->
	V2 = redis_sd:any_to_binary(val(Rec, V)),
	S?REDIS_SD_SERVICE{rec=Rec?REDIS_SD_DNS{target=V2}};
setval(txtdata, S=?REDIS_SD_SERVICE{txtdata=TXTData, rec=Rec}) ->
	V2 = [{val(Rec, K), val(Rec, V)} || {K, V} <- val(Rec, TXTData)],
	S?REDIS_SD_SERVICE{rec=Rec?REDIS_SD_DNS{txtdata=V2}};
setval(keys, S=?REDIS_SD_SERVICE{rec=?REDIS_SD_DNS{domain=Domain, type=Type, service=Service, instance=Instance}}) ->
	Keys = redis_sd:labels_to_keys({Domain, Type, Service, Instance}),
	PTR = proplists:get_value(ptr, Keys),
	SRV = proplists:get_value(srv, Keys),
	KEY = proplists:get_value(key, Keys),
	S?REDIS_SD_SERVICE{ptr=PTR, srv=SRV, key=KEY}.

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

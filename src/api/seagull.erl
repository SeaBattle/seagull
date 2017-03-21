%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Oct 2016 20:12
%%%-------------------------------------------------------------------
-module(seagull).
-author("tihon").

-define(DEFAULT_CONSUL_DNS_PORT, 8600).
-define(LOCAL_CONSUL_IP, "127.0.0.1").

%% conf holder api
-export([add_backend/2, add_backend/3, add_callback/2, remove_callback/1]).

%% unified api
-export([get_service/1, get_services/0, get_value/1, set_value/2, get_value/2, drop_value/1]).

%% consul api
-export([register/4, deregister/2, get_service_near/1, get_service_near/2, dns_request/1, dns_request/2, dns_request/3]).

%% etcd api
-export([register/3, deregister/1]).

-spec add_backend(module(), string()) -> boolean().
add_backend(Module, Url) ->
  add_backend(Module, Url, []).

-spec add_backend(module(), string(), poplists:proplist()) -> boolean().
add_backend(Module, Url, Opts) ->
  sc_backend_man:register_backend(Module, Url, Opts).

%% conf holder api
-spec add_callback(binary(), fun()) -> true.
add_callback(Var, Fun) when is_binary(Var), is_function(Fun) ->
  erlang:error(not_implemented).

-spec remove_callback(binary()) -> ok.
remove_callback(_Var) ->
  erlang:error(not_implemented).

%% unified api
-spec get_service(string()) -> {ok, map()} | {ok, list(map())} | undefined | {error, any()}.
get_service(Service) ->
  sc_backend_man:request_backend(get_service, [Service]).

-spec get_services() -> {ok, map()} | {error, any()}.
get_services() ->
  sc_backend_man:request_backend(get_services).

-spec register(string(), string(), integer()) -> ok | {error, any()}.
register(Service, Addr, Port) ->
  register(Service, Addr, Port, undefined).

-spec register(string(), string(), integer(), string() | undefined) -> ok | {error, any()}.
register(Service, Addr, Port, Node) ->
  sc_backend_man:request_backend(register, [Service, Addr, Port, Node]).

-spec deregister(string()) -> ok | {error, any()}.
deregister(Service) -> deregister(Service, undefined).

-spec deregister(string(), string() | undefined) -> ok | {error, any()}.
deregister(Service, Node) ->
  sc_backend_man:request_backend(deregister, [Service, Node]).

-spec get_value(binary()) -> binary() | undefined | {error, any()}.
get_value(Key) ->
  sc_backend_man:request_backend(get_value, [Key]).

-spec get_value(binary(), _Default :: any()) -> binary() | _Default | {error, any()}.
get_value(Key, Default) ->
  case get_value(Key) of
    undefined -> Default;
    Value -> Value
  end.

-spec set_value(binary(), binary()) -> ok | {error, any()}.
set_value(Key, Value) ->
  sc_backend_man:request_backend(set_value, [Key, Value]).

-spec drop_value(binary()) -> ok | {error, any()}.
drop_value(Key) ->
  sc_backend_man:request_backend(drop_value, [Key]).

%% ------------ Consul specific ------------
-spec get_service_near(string()) -> {ok, map(), list(map())} | {error, any()}.
get_service_near(Service) ->
  {ok, Node} = inet:gethostname(),
  get_service_near(Service, Node).

-spec get_service_near(string(), string()) -> {ok, map(), list(map())} | {error, any()}.
get_service_near(Service, Node) ->
  case sc_conf_holder:get_service_conf() of
    undefined -> throw(no_conf);
    {sc_backend_consul, Url, _} ->
      sc_backend_consul:get_service_near(Url, Service, Node);
    {_, _} -> throw(wrong_backend)
  end.

-spec dns_request(string()) -> list().
dns_request(Service) ->
  dns_request(Service, ?LOCAL_CONSUL_IP).

-spec dns_request(string(), string()) -> list().
dns_request(Service, Ip) ->
  dns_request(Service, Ip, ?DEFAULT_CONSUL_DNS_PORT).

-spec dns_request(string(), string(), pos_integer()) -> list().
dns_request(Service, Ip, Port) ->
  sc_backend_consul:dns_request(Service, Ip, Port).
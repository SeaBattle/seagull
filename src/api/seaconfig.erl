%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Oct 2016 20:12
%%%-------------------------------------------------------------------
-module(seaconfig).
-author("tihon").

-define(DEFAULT_CONSUL_DNS_PORT, 8600).
-define(LOCAL_CONSUL_IP, "127.0.0.1").

%% conf holder api
-export([add_callback/2, remove_callback/1]).

%% unified api
-export([get_service/1, get_services/0, register/3, get_value/1, set_value/2]).

%% consul api
-export([get_service_near/1, get_service_near/2, dns_request/1, dns_request/2, dns_request/3]).

%% conf holder api
-spec add_callback(binary(), fun()) -> true.
add_callback(Var, Fun) when is_binary(Var), is_function(Fun) ->
  erlang:error(not_implemented).

-spec remove_callback(binary()) -> ok.
remove_callback(_Var) ->
  erlang:error(not_implemented).

%% unified api
get_service(Service) ->
  sc_backend_man:request_backend(get_service, [Service]).

get_services() ->
  sc_backend_man:request_backend(get_services).

%% consul api
get_service_near(Service) ->
  {ok, Node} = inet:gethostname(),
  get_service_near(Service, Node).

get_service_near(Service, Node) ->
  case sc_conf_holder:get_service_conf() of
    undefined -> throw(no_conf);
    {consul, Url} ->
      sc_backend_consul:get_service_near(Url, Service, Node);
    {_, _} -> throw(wrong_backend)
  end.

register(Service, Addr, Port) ->
  sc_backend_man:request_backend(register, [Service, Addr, Port]).

get_value(Key) ->
  sc_backend_man:request_backend(get_value, [Key]).

set_value(Key, Value) ->
  sc_backend_man:request_backend(set_value, [Key, Value]).

dns_request(Service) ->
  dns_request(Service, ?LOCAL_CONSUL_IP).

dns_request(Service, Ip) ->
  dns_request(Service, Ip, ?DEFAULT_CONSUL_DNS_PORT).

-spec dns_request(string(), string(), pos_integer()) -> list().
dns_request(Service, Ip, Port) ->
  sc_backend_consul:dns_request(Service, Ip, Port).
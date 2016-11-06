%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Oct 2016 18:15
%%%-------------------------------------------------------------------
-module(sc_backend_consul).
-author("tihon").

-behavior(sc_backend).

%% Callbacks
-export([get_service/2, get_services/1]).

%% Consul API
-export([dns_request/3, get_service_near/3]).

-define(SERVICES, "~s/v1/catalog/services/").
-define(SERVICE, "~s/v1/catalog/service/~s").
-define(SERVICE_NEAR, "~s/v1/catalog/service/~s?near=~s").

-define(MAP, [{object_format, map}]).

-spec get_service(string(), string()) -> {ok, map()} | {error, any()}.
get_service(Addr, Name) ->
  Url = io_lib:format(?SERVICE, [Addr, Name]),
  case httpc:request(get, {Url, []}, [], [{body_format, binary}]) of
    {ok, {{_, 200, _}, _, Reply}} ->
      {ok, jsone:decode(Reply, ?MAP)};
    Err ->
      {error, Err}
  end.

-spec get_services(string()) -> {ok, map()} | {error, any()}.
get_services(Addr) ->
  Url = io_lib:format(?SERVICES, [Addr]),
  case httpc:request(get, {Url, []}, [], [{body_format, binary}]) of
    {ok, {{_, 200, _}, _, Reply}} ->
      {ok, jsone:decode(Reply, ?MAP)};
    Err ->
      {error, Err}
  end.

-spec get_service_near(string(), string(), string()) -> {ok, map(), list()} | {error, any()}.
get_service_near(Addr, Name, Node) ->
  Url = io_lib:format(?SERVICE_NEAR, [Addr, Name, Node]),
  case httpc:request(get, {Url, []}, [], [{body_format, binary}]) of
    {ok, {{_, 200, _}, _, Reply}} ->
      [Near | Other] = jsone:decode(Reply, ?MAP),
      {ok, Near, Other};
    Err ->
      {error, Err}
  end.

-spec dns_request(string(), string(), pos_integer()) -> list().
dns_request(Service, ConsulIp, Port) ->
  {ok, Ip} = inet:parse_address(ConsulIp),
  inet_res:lookup(Service ++ ".service.consul", any, srv, [{nameservers, [{Ip, Port}]}]).
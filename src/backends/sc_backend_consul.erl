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
-export([get_service/2, get_services/1, register/5, get_value/2, set_value/3, deregister/3, drop_value/2]).

%% Consul API
-export([dns_request/3, get_service_near/3]).

-define(TEXT, "application/text").
-define(JSON, "application/json").
-define(SERVICES, "~s/v1/catalog/services").
-define(SERVICE, "~s/v1/catalog/service/~s").
-define(REGISTER, "~s/v1/catalog/register").
-define(DEREGISTER, "~s/v1/catalog/deregister").
-define(SERVICE_NEAR, "~s/v1/catalog/service/~s?near=~s").
-define(KEYVALUEREC, "~s/v1/kv/~s?recurse").
-define(KEYVALUE, "~s/v1/kv/~s").

-define(MAP, [{object_format, map}]).

-spec get_service(string(), string()) -> {ok, map()} | {ok, map()} | {ok, list(map())} |undefined | {error, any()}.
get_service(Addr, Name) ->
  Url = lists:flatten(io_lib:format(?SERVICE, [Addr, Name])),
  case http_get(Url) of
    {ok, []} -> undefined;
    {ok, [Map]} -> {ok, Map};
    Other -> Other  %{ok, list(map())} | {error, any()}
  end.

-spec get_services(string()) -> {ok, map()} | {error, any()}.
get_services(Addr) ->
  Url = lists:flatten(io_lib:format(?SERVICES, [Addr])),
  http_get(Url).

-spec get_service_near(string(), string(), string()) -> {ok, map(), list()} | {error, any()}.
get_service_near(Addr, Name, Node) ->
  Url = lists:flatten(io_lib:format(?SERVICE_NEAR, [Addr, Name, Node])),
  case http_get(Url) of
    {ok, [Near | Other]} -> {ok, Near, Other};
    Other -> Other  %error | undefined
  end.

-spec dns_request(string(), string(), pos_integer()) -> list().
dns_request(Service, ConsulIp, Port) ->
  {ok, Ip} = inet:parse_address(ConsulIp),
  inet_res:lookup(Service ++ ".service.consul", any, srv, [{nameservers, [{Ip, Port}]}]).

-spec register(string(), string(), string(), integer(), string() | undefined) ->
  ok | {error, any()}.
register(Host, Service, Address, Port, undefined) ->
  {ok, Node} = inet:gethostname(),
  register(Host, Service, Address, Port, Node);
register(Host, Service, Address, Port, Node) ->
  Url = lists:flatten(io_lib:format(?REGISTER, [Host])),
  Body = form_register_body(Service, Address, Port, Node),
  http_put(Url, ?JSON, Body).

-spec deregister(string(), string(), string() | undefined) ->
  ok | {error, any()}.
deregister(Host, Service, undefined) ->
  {ok, Node} = inet:gethostname(),
  deregister(Host, Service, Node);
deregister(Host, Service, Node) ->
  Url = lists:flatten(io_lib:format(?DEREGISTER, [Host])),
  Body = form_deregister_body(Service, Node),
  http_put(Url, ?JSON, Body).

-spec get_value(string(), binary()) ->
  binary() | proplists:proplist() | undefined | {error, any()}.
get_value(Host, Key) ->
  Url = lists:flatten(io_lib:format(?KEYVALUEREC, [Host, Key])),
  case http_get(Url) of
    {ok, [Value]} -> get_value(Value);
    {ok, Values} -> get_values(Values);
    Other -> Other  %error | undefined
  end.

-spec set_value(string(), binary(), binary()) -> ok | {error, any()}.
set_value(Host, Key, Value) ->
  Url = lists:flatten(io_lib:format(?KEYVALUE, [Host, Key])),
  http_put(Url, ?TEXT, Value).

-spec drop_value(string(), binary()) -> ok | {error, any()}.
drop_value(Host, Key) ->
  Url = lists:flatten(io_lib:format(?KEYVALUEREC, [Host, Key])),
  case httpc:request(delete, {Url, []}, [], [{body_format, binary}]) of
    {ok, {{_, 200, _}, _, <<"true">>}} ->
      ok;
    Err ->
      {error, Err}
  end.

%% @private
form_register_body(Service, Address, Port, Node) ->
  Adr = list_to_binary(Address),
  jsone:encode(#{
    <<"Address">> => Adr,
    <<"Node">> => list_to_binary(Node),
    <<"Service">> =>
    #{
      <<"Service">> => list_to_binary(Service),
      <<"Address">> => Adr,
      <<"Port">> => Port
    }
  }).

%% @private
form_deregister_body(Service, Node) ->
  jsone:encode(#{
    <<"Node">> => list_to_binary(Node),
    <<"ServiceID">> => list_to_binary(Service)
  }).

%% @private
http_get(Url) ->
  case httpc:request(get, {Url, []}, [], [{body_format, binary}]) of
    {ok, {{_, 200, _}, _, Reply}} ->
      {ok, jsone:decode(Reply, ?MAP)};
    {ok, {{_, 404, _}, _, _}} ->
      undefined;
    Err ->
      {error, Err}
  end.

%% @private
http_put(Url, Type, Body) ->
  case httpc:request(put, {Url, [], Type, Body}, [], [{body_format, binary}]) of
    {ok, {{_, 200, _}, _, _}} -> ok;
    Err -> {error, Err}
  end.

%% @private
get_values(Values) ->
  lists:foldl(
    fun
      (#{<<"Value">> := null}, Acc) -> Acc;  % directory
      (Value = #{<<"Key">> := Key}, Acc) ->  % value
        [{Key, get_value(Value)} | Acc]
    end, [], Values).

%% @private
get_value(#{<<"Value">> := null}) -> undefined;
get_value(#{<<"Value">> := Value}) -> base64:decode(Value).
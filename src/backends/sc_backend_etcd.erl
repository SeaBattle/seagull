%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Oct 2016 18:15
%%%-------------------------------------------------------------------
-module(sc_backend_etcd).
-author("tihon").

-behavior(sc_backend).

-define(NOT_FOUND, 100).
-define(ALREADY_CREATED, 102).
-define(SERVICE, "~s/v2/keys/~s").
-define(SERVICES, "~s/v2/keys").
-define(ADD_SERVICE, "~s/v2/keys/~s/~s").
-define(SERVICE_KEY, "/~s:~s:~p").
-define(SERVICE_VALUE, "value=:~p").
-define(KEYVALUE, "~s/v2/keys/kv/~s").
-define(DELETE_DIR, "~s/v2/keys/~s/?recursive=true").
-define(MAP, [{object_format, map}]).

%% API
-export([get_service/2, get_services/1, register/5, get_value/2, set_value/3, deregister/3]).

-spec get_service(string(), string()) -> {ok, map()} | {error, any()}.
get_service(Host, Name) ->
  Url = lists:flatten(io_lib:format(?SERVICE, [Host, Name])),
  case http_get(Url) of
    {ok, #{<<"errorCode">> := ?NOT_FOUND}} -> undefined;
    {ok, #{<<"node">> := Node}} -> {ok, Node};
    Err -> {error, Err}
  end.

-spec get_services(string()) -> {ok, map()} | {error, any()}.
get_services(Host) ->
  Url = lists:flatten(io_lib:format(?SERVICES, [Host])),
  case http_get(Url) of
    {ok, #{<<"node">> := #{<<"dir">> := true, <<"nodes">> := Nodes}}} ->
      {ok, prepare_servises(Nodes)};
    Err ->
      {error, Err}
  end.

-spec register(string(), string(), undefined, string(), integer()) -> ok | {error, any()}.
register(Host, Service, _, Address, Port) ->
  Key = lists:flatten(io_lib:format(?SERVICE_KEY, [Address, Service, Port])),
  Url = lists:flatten(io_lib:format(?ADD_SERVICE, [Host, Service, Key])),
  Value = lists:flatten(io_lib:format(?SERVICE_VALUE, [Port])),
  http_put(Url, Value).

-spec deregister(string(), string(), undefined) -> ok | {error, any()}.
deregister(Host, Service, _) ->
  Url = lists:flatten(io_lib:format(?DELETE_DIR, [Host, Service])),
  case httpc:request(delete, {Url, []}, [], []) of
    {ok, {{_, 200, _}, _, Reply}} ->
      case jsone:decode(Reply, ?MAP) of
        #{<<"action">> := <<"delete">>} ->
          ok;
        #{<<"errorCode">> := Code, <<"message">> := Msg} ->
          {error, {Code, Msg}}
      end;
    {ok, {{_, 404, _}, _, _}} ->
      undefined;
    Err ->
      {error, Err}
  end.

-spec get_value(string(), binary()) -> binary() | undefined | {error, any()}.
get_value(Host, Key) ->
  Url = lists:flatten(io_lib:format(?KEYVALUE, [Host, Key])),
  case http_get(Url) of
    {ok, #{<<"errorCode">> := ?NOT_FOUND}} -> undefined;
    {ok, #{<<"node">> := #{<<"value">> := Value}}} -> {ok, Value};
    Err -> {error, Err}
  end.

-spec set_value(string(), binary(), binary()) -> ok | {error, any()}.
set_value(Host, Key, Value) ->
  Url = lists:flatten(io_lib:format(?KEYVALUE, [Host, Key])),
  http_put(Url, Value).


%% @private
prepare_servises(Nodes) ->
  lists:map(fun(#{<<"key">> := Key}) -> #{Key => []} end, Nodes).

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
http_put(Url, Data) ->
  case httpc:request(put, {Url, [], "application/x-www-form-urlencoded", Data}, [], []) of
    {ok, {{_, 200, _}, _, Reply}} ->
      case jsone:decode(Reply, ?MAP) of
        #{<<"errorCode">> := Code, <<"message">> := Msg} -> {error, {Code, Msg}};
        #{<<"action">> := <<"set">>} -> ok
      end;
    Err ->
      {error, Err}
  end.
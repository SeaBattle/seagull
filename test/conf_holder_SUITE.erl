%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jan 2017 19:40
%%%-------------------------------------------------------------------
-module(conf_holder_SUITE).
-author("tihon").

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(CONSUL_URL, "http://127.0.0.1:8500").

all() ->
  [
    test_auto_register,
    test_auto_update,
    test_caching_on_get,
    test_update_on_set
  ].

init_per_suite(Config) ->
  application:ensure_all_started(inets),
  Config.

end_per_suite(_Config) ->
  ok.

init_per_testcase(_Case, Config) ->
  Config.

end_per_testcase(_Case, Config) ->
  Config.

%% Tests
test_auto_register(Config) ->
  {ok, _} = sc_conf_holder:start_link(),

  undefined = try seaconfig:get_service("my_service")
              catch throw:no_conf -> undefined
              end,

  ?assert(sc_conf_holder:register_backend(sc_backend_consul, ?CONSUL_URL,
    [{autoregister, #{service => "my_service", address => "127.0.0.1", port => 4503}}])),

  {ok, Service} = seaconfig:get_service("my_service"),

  {ok, Node} = inet:gethostname(),
  NodeBin = list_to_binary(Node),

  #{<<"Address">> := <<"127.0.0.1">>,
    <<"Node">> := NodeBin,
    <<"ServiceAddress">> := <<"127.0.0.1">>,
    <<"ServiceID">> := <<"my_service">>,
    <<"ServiceName">> := <<"my_service">>,
    <<"ServicePort">> := 4503} = Service,

  ok = seaconfig:deregister("my_service"),
  Config.

test_auto_update(Config) ->
  {ok, _} = sc_conf_holder:start_link(),

  ok = http_put(<<"key1">>, <<"value1">>),
  ok = http_put(<<"key2">>, <<"value2">>),

  ?assert(sc_conf_holder:register_backend(sc_backend_consul, ?CONSUL_URL,
    [{cache, #{enable => true, update_time => 10}}])),

  timer:sleep(10),
  [] = ets:lookup(sc_conf, <<"key1">>), % no value as it was not selected first

  ?assertEqual(<<"value1">>, seaconfig:get_value(<<"key1">>)),

  [{<<"key1">>, <<"value1">>}] = ets:lookup(sc_conf, <<"key1">>), % no value as it was not selected first

  ok = http_put(<<"key1">>, <<"modified">>),

  timer:sleep(10),

  [{<<"key1">>, <<"modified">>}] = ets:lookup(sc_conf, <<"key1">>), % value was modified

  ?assertEqual(<<"modified">>, seaconfig:get_value(<<"key1">>)),

  ok = seaconfig:drop_value(<<"key1">>),
  ok = seaconfig:drop_value(<<"key2">>),

  Config.

test_caching_on_get(Config) ->
  {ok, _} = sc_conf_holder:start_link(),

  ok = http_put(<<"key1">>, <<"value1">>),
  ok = http_put(<<"key2">>, <<"value2">>),

  Self = self(),
  meck:expect(sc_backend_consul, get_value, fun(A, B) -> Self ! {get_value, A, B}, meck:passthrough([A, B]) end),

  ?assert(sc_conf_holder:register_backend(sc_backend_consul, ?CONSUL_URL,
    [{cache, #{enable => true, update_time => undefined}}])),

  ?assertEqual(<<"value1">>, seaconfig:get_value(<<"key1">>)),
  true = receive
           {get_value, _, <<"key1">>} -> true
         after 100 -> false
         end,

  ?assertEqual(<<"value1">>, seaconfig:get_value(<<"key1">>)),
%%  second time value was fetched from cache
  false = receive
           {get_value, _, <<"key1">>} -> true
         after 100 -> false
         end,

  meck:unload(),
  ok = seaconfig:drop_value(<<"key1">>),
  ok = seaconfig:drop_value(<<"key2">>),

  Config.

test_update_on_set(Config) ->
  {ok, _} = sc_conf_holder:start_link(),

  ok = http_put(<<"key1">>, <<"value1">>),
  ok = http_put(<<"key2">>, <<"value2">>),

  ?assert(sc_conf_holder:register_backend(sc_backend_consul, ?CONSUL_URL,
    [{cache, #{enable => true, update_time => undefined}}])),

  ?assertEqual(<<"value1">>, seaconfig:get_value(<<"key1">>)),

  [{<<"key1">>, <<"value1">>}] = ets:lookup(sc_conf, <<"key1">>), % value in cache

  ok = seaconfig:set_value(<<"key1">>, <<"new_value">>),

  [{<<"key1">>, <<"new_value">>}] = ets:lookup(sc_conf, <<"key1">>), % value was modified

  ok = seaconfig:drop_value(<<"key1">>),
  ok = seaconfig:drop_value(<<"key2">>),

  Config.


%% @private
http_put(Key, Value) ->
  Url = lists:flatten(io_lib:format("http://127.0.0.1:8500/v1/kv/~s", [Key])),
  case httpc:request(put, {Url, [], "application/text", Value}, [], [{body_format, binary}]) of
    {ok, {{_, 200, _}, _, _}} -> ok;
    Err -> {error, Err}
  end.
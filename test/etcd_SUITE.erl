%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jan 2017 19:37
%%%-------------------------------------------------------------------
-module(etcd_SUITE).
-author("tihon").

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(ETCD_URL, "http://127.0.0.1:2379").
-define(ETCD_CHECK_URL, "http://127.0.0.1:2379/version").
-define(TEST_SERVICE1, "test_service1").
-define(TEST_SERVICE2, "test_service2").

all() ->
  [
    test_register,
    test_get_service,
    test_get_services,
    test_set_kv,
    test_get_kv
  ].

init_per_suite(Config) ->
  application:ensure_all_started(inets),
  ?assert(ensure_etcd()),
  Config.

end_per_suite(_Config) ->
  ok.

init_per_testcase(_Case, Config) ->
  Config.

end_per_testcase(_Case, Config) ->
  Config.

%% Tests
test_register(Config) ->
  start_etcd(),

  ok = seaconfig:register(?TEST_SERVICE1, "127.0.0.1", 4232, "Node1"),
  {ok, Services} = seaconfig:get_services(),
  ?assert(maps:is_key(list_to_binary(?TEST_SERVICE1), Services)),
  {ok, Service} = seaconfig:get_service(?TEST_SERVICE1),
  #{<<"key">> := <<"test_service1">>,
    <<"nodes">> := [
      #{<<"key">> := <<"/test_service1/127.0.0.1:test_service1:4232">>,
        <<"value">> := <<":4232">>}]} = Service,

  ok = seaconfig:deregister(?TEST_SERVICE1, "Node1"),
  {ok, UServices} = seaconfig:get_services(),
  ?assertNot(maps:is_key(list_to_binary(?TEST_SERVICE1), UServices)),
  undefined = seaconfig:get_service(?TEST_SERVICE1),

  Config.

test_get_service(Config) ->
  start_etcd(),

  undefined = seaconfig:get_service(?TEST_SERVICE1),

  ok = seaconfig:register(?TEST_SERVICE1, "127.0.0.1", 4232, "Node1"),
  ok = seaconfig:register(?TEST_SERVICE2, "127.0.0.1", 4232, "Node1"),
  {ok, Service1} = seaconfig:get_service(?TEST_SERVICE1),
  #{<<"key">> := <<"test_service1">>,
    <<"nodes">> := [
      #{<<"key">> := <<"/test_service1/127.0.0.1:test_service1:4232">>,
        <<"value">> := <<":4232">>}]} = Service1,

  {ok, Service2} = seaconfig:get_service(?TEST_SERVICE2),
  #{<<"key">> := <<"test_service2">>,
    <<"nodes">> := [
      #{<<"key">> := <<"/test_service2/127.0.0.1:test_service2:4232">>,
        <<"value">> := <<":4232">>}]} = Service2,

  ok = seaconfig:deregister(?TEST_SERVICE1, "Node1"),
  ok = seaconfig:deregister(?TEST_SERVICE2, "Node1"),

  undefined = seaconfig:get_service(?TEST_SERVICE1),
  undefined = seaconfig:get_service(?TEST_SERVICE2),

  Config.

test_get_services(Config) ->
  start_etcd(),

  ok = seaconfig:register(?TEST_SERVICE1, "127.0.0.1", 4232),
  ok = seaconfig:register(?TEST_SERVICE2, "127.0.0.1", 4232),

  {ok, Services} = seaconfig:get_services(),
  ?assertEqual(
    #{<<"test_service1">> => [], <<"test_service2">> => []}, Services),

  ok = seaconfig:deregister(?TEST_SERVICE2, "Node1"),  % service 2 is out
  {ok, Services2} = seaconfig:get_services(),
  ?assertEqual(
    #{<<"test_service1">> => []}, Services2),

  ok = seaconfig:deregister(?TEST_SERVICE1, "Node1"), % service 1 is out
  {ok, Services4} = seaconfig:get_services(),
  ?assertEqual(0, map_size(Services4)),

  Config.

test_set_kv(Config) ->
  start_etcd(),

  Key = <<"key1">>,
  Value = <<"value1">>,

  ok = seaconfig:set_value(Key, Value),
  Value = seaconfig:get_value(Key),

  ok = seaconfig:set_value(Key, <<"other_value">>),
  <<"other_value">> = seaconfig:get_value(Key),

  ok = seaconfig:drop_value(Key),

  Config.

test_get_kv(Config) ->
  start_etcd(),

  Key = <<"key1">>,
  Value = <<"value1">>,

  undefined = seaconfig:get_value(Key),

  ok = seaconfig:set_value(Key, Value),
  Value = seaconfig:get_value(Key),

  ok = seaconfig:drop_value(Key),
  undefined = seaconfig:get_value(Key),

  Config.


%% @private
ensure_etcd() ->
  case httpc:request(get, {?ETCD_CHECK_URL, []}, [], []) of
    {ok, {{_, 200, _}, _, _}} ->
      true;
    Err ->
      {false, Err}
  end.

%% @private
start_etcd() ->
  {ok, _} = sc_conf_holder:start_link(),
  ?assert(sc_conf_holder:register_backend(sc_backend_etcd, ?ETCD_URL, [])).
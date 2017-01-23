%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jan 2017 19:37
%%%-------------------------------------------------------------------
-module(consull_SUITE).
-author("tihon").

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(CONSUL_URL, "http://127.0.0.1:8500").
-define(CONSUL_CHECK_URL, "http://127.0.0.1:8500/v1/status/leader").
-define(TEST_SERVICE1, "test_service1").
-define(TEST_SERVICE2, "test_service2").

all() ->
  [
    test_register,
    test_get_service,
    test_get_services,
    test_get_services_near,
    test_get_dns_request,
    test_set_kv,
    test_get_kv
  ].

init_per_suite(Config) ->
  application:ensure_all_started(inets),
  ?assert(ensure_consul()),
  Config.

end_per_suite(_Config) ->
  ok.

init_per_testcase(_Case, Config) ->
  Config.

end_per_testcase(_Case, Config) ->
  Config.

%% Tests
test_register(Config) ->
  start_consul(),

  ok = seaconfig:register(?TEST_SERVICE1, "Node1", "127.0.0.1", 4232),
  {ok, Services} = seaconfig:get_services(),
  ?assert(maps:is_key(list_to_binary(?TEST_SERVICE1), Services)),
  {ok, Service} = seaconfig:get_service(?TEST_SERVICE1),
  #{<<"Address">> := <<"127.0.0.1">>,
    <<"Node">> := <<"Node1">>,
    <<"ServiceAddress">> := <<"127.0.0.1">>,
    <<"ServiceID">> := <<"test_service1">>,
    <<"ServiceName">> := <<"test_service1">>,
    <<"ServicePort">> := 4232} = Service,

  ok = seaconfig:deregister(?TEST_SERVICE1, "Node1"),
  {ok, UServices} = seaconfig:get_services(),
  ?assertNot(maps:is_key(list_to_binary(?TEST_SERVICE1), UServices)),
  undefined = seaconfig:get_service(?TEST_SERVICE1),
  Config.

test_get_service(Config) ->
  start_consul(),

  undefined = seaconfig:get_service(?TEST_SERVICE1),

  ok = seaconfig:register(?TEST_SERVICE1, "Node1", "127.0.0.1", 4232),
  ok = seaconfig:register(?TEST_SERVICE2, "Node1", "127.0.0.1", 4232),
  {ok, Service1} = seaconfig:get_service(?TEST_SERVICE1),
  #{<<"Address">> := <<"127.0.0.1">>,
    <<"Node">> := <<"Node1">>,
    <<"ServiceAddress">> := <<"127.0.0.1">>,
    <<"ServiceID">> := <<"test_service1">>,
    <<"ServiceName">> := <<"test_service1">>,
    <<"ServicePort">> := 4232} = Service1,

  ok = seaconfig:register(?TEST_SERVICE1, "Node2", "127.0.0.1", 4232),
  {ok, Service12} = seaconfig:get_service(?TEST_SERVICE1),

  [#{<<"Address">> := <<"127.0.0.1">>,
    <<"Node">> := <<"Node1">>,
    <<"ServiceAddress">> := <<"127.0.0.1">>,
    <<"ServiceID">> := <<"test_service1">>,
    <<"ServiceName">> := <<"test_service1">>,
    <<"ServicePort">> := 4232},
    #{<<"Address">> := <<"127.0.0.1">>,
      <<"Node">> := <<"Node2">>,
      <<"ServiceAddress">> := <<"127.0.0.1">>,
      <<"ServiceID">> := <<"test_service1">>,
      <<"ServiceName">> := <<"test_service1">>}] = Service12,

  {ok, Service2} = seaconfig:get_service(?TEST_SERVICE2),
  #{<<"Address">> := <<"127.0.0.1">>,
    <<"Node">> := <<"Node1">>,
    <<"ServiceAddress">> := <<"127.0.0.1">>,
    <<"ServiceID">> := <<"test_service2">>,
    <<"ServiceName">> := <<"test_service2">>,
    <<"ServicePort">> := 4232} = Service2,

  ok = seaconfig:deregister(?TEST_SERVICE1, "Node1"),
  ok = seaconfig:deregister(?TEST_SERVICE1, "Node2"),
  ok = seaconfig:deregister(?TEST_SERVICE2, "Node1"),

  undefined = seaconfig:get_service(?TEST_SERVICE1),
  undefined = seaconfig:get_service(?TEST_SERVICE2),

  Config.

test_get_services(Config) ->
  start_consul(),

  ok = seaconfig:register(?TEST_SERVICE1, "Node1", "127.0.0.1", 4232),
  ok = seaconfig:register(?TEST_SERVICE2, "Node1", "127.0.0.1", 4232),
  ok = seaconfig:register(?TEST_SERVICE2, "Node2", "127.0.0.1", 5131),

  {ok, Services} = seaconfig:get_services(),
  ?assertEqual(#{<<"consul">> => [], <<"test_service1">> => [], <<"test_service2">> => []}, Services),

  ok = seaconfig:deregister(?TEST_SERVICE2, "Node1"), % service 2 still lives on Node2
  {ok, Services2} = seaconfig:get_services(),
  ?assertEqual(#{<<"consul">> => [], <<"test_service1">> => [], <<"test_service2">> => []}, Services2),

  ok = seaconfig:deregister(?TEST_SERVICE2, "Node2"), % service 2 is out
  {ok, Services3} = seaconfig:get_services(),
  ?assertEqual(#{<<"consul">> => [], <<"test_service1">> => []}, Services3),

  ok = seaconfig:deregister(?TEST_SERVICE1, "Node1"), % service 1 is out
  {ok, Services4} = seaconfig:get_services(),
  ?assertEqual(#{<<"consul">> => []}, Services4),

  Config.

test_get_services_near(Config) ->
  start_consul(),

  ok = seaconfig:register(?TEST_SERVICE1, undefined, "127.0.0.1", 4232),
  ok = seaconfig:register(?TEST_SERVICE1, "Node1", "127.0.0.1", 4232),
  ok = seaconfig:register(?TEST_SERVICE2, "Node2", "127.0.0.1", 5131),

  {ok, Service, Other} = seaconfig:get_service_near(?TEST_SERVICE1),
  {ok, Node} = inet:gethostname(),
  NodeBin = list_to_binary(Node),
  #{<<"Node">> := <<"Node1">>} = Service,
  [#{<<"Node">> := NodeBin}] = Other,

  ok = seaconfig:deregister(?TEST_SERVICE1, "Node1"),
  ok = seaconfig:deregister(?TEST_SERVICE1, undefined),
  ok = seaconfig:deregister(?TEST_SERVICE2, "Node2"),

  Config.

test_get_dns_request(Config) ->
  start_consul(),

  ok = seaconfig:register(?TEST_SERVICE1, undefined, "127.0.0.1", 4232),
  ok = seaconfig:register(?TEST_SERVICE1, "Node1", "127.0.0.1", 4232),
  ok = seaconfig:register(?TEST_SERVICE2, undefined, "127.0.0.1", 5131),

  Services = seaconfig:dns_request(?TEST_SERVICE1),
  {ok, Node} = inet:gethostname(),

  ?assert(lists:member({1, 1, 4232, "Node1.node.dc1.consul"}, Services)),
  ?assert(lists:member({1, 1, 4232, Node ++ ".node.dc1.consul"}, Services)),
  ?assert(length(Services) == 2),

  ok = seaconfig:deregister(?TEST_SERVICE1, "Node1"),
  ok = seaconfig:deregister(?TEST_SERVICE1, undefined),
  ok = seaconfig:deregister(?TEST_SERVICE2, undefined),

  Config.

test_set_kv(Config) ->
  start_consul(),

  Key = <<"key1">>,
  Value = <<"value1">>,

  ok = seaconfig:set_value(Key, Value),
  Value = seaconfig:get_value(Key),

  ok = seaconfig:set_value(Key, <<"other_value">>),
  <<"other_value">> = seaconfig:get_value(Key),

  ok = seaconfig:drop_value(Key),

  Config.

test_get_kv(Config) ->
  start_consul(),

  Key = <<"key1">>,
  Value = <<"value1">>,

  undefined = seaconfig:get_value(Key),

  ok = seaconfig:set_value(Key, Value),
  Value = seaconfig:get_value(Key),

  ok = seaconfig:drop_value(Key),
  undefined = seaconfig:get_value(Key),

  Config.


%% @private
ensure_consul() ->
  case httpc:request(get, {?CONSUL_CHECK_URL, []}, [], []) of
    {ok, {{_, 200, _}, _, _}} ->
      true;
    Err ->
      {false, Err}
  end.

%% @private
start_consul() ->
  {ok, _} = sc_conf_holder:start_link(),
  ?assert(sc_conf_holder:register_backend(sc_backend_consul, ?CONSUL_URL, [])).
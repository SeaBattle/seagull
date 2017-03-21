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

  ok = seagull:register(?TEST_SERVICE1, "127.0.0.1", 4232, "Node1"),
  {ok, Services} = seagull:get_services(),
  ?assert(maps:is_key(list_to_binary(?TEST_SERVICE1), Services)),
  {ok, Service} = seagull:get_service(?TEST_SERVICE1),
  #{<<"Address">> := <<"127.0.0.1">>,
    <<"Node">> := <<"Node1">>,
    <<"ServiceAddress">> := <<"127.0.0.1">>,
    <<"ServiceID">> := <<"test_service1">>,
    <<"ServiceName">> := <<"test_service1">>,
    <<"ServicePort">> := 4232} = Service,

  ok = seagull:deregister(?TEST_SERVICE1, "Node1"),
  {ok, UServices} = seagull:get_services(),
  ?assertNot(maps:is_key(list_to_binary(?TEST_SERVICE1), UServices)),
  undefined = seagull:get_service(?TEST_SERVICE1),
  Config.

test_get_service(Config) ->
  start_consul(),

  undefined = seagull:get_service(?TEST_SERVICE1),

  ok = seagull:register(?TEST_SERVICE1, "127.0.0.1", 4232, "Node1"),
  ok = seagull:register(?TEST_SERVICE2, "127.0.0.1", 4232, "Node1"),
  {ok, Service1} = seagull:get_service(?TEST_SERVICE1),
  #{<<"Address">> := <<"127.0.0.1">>,
    <<"Node">> := <<"Node1">>,
    <<"ServiceAddress">> := <<"127.0.0.1">>,
    <<"ServiceID">> := <<"test_service1">>,
    <<"ServiceName">> := <<"test_service1">>,
    <<"ServicePort">> := 4232} = Service1,

  ok = seagull:register(?TEST_SERVICE1, "127.0.0.1", 4232, "Node2"),
  {ok, Service12} = seagull:get_service(?TEST_SERVICE1),

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

  {ok, Service2} = seagull:get_service(?TEST_SERVICE2),
  #{<<"Address">> := <<"127.0.0.1">>,
    <<"Node">> := <<"Node1">>,
    <<"ServiceAddress">> := <<"127.0.0.1">>,
    <<"ServiceID">> := <<"test_service2">>,
    <<"ServiceName">> := <<"test_service2">>,
    <<"ServicePort">> := 4232} = Service2,

  ok = seagull:deregister(?TEST_SERVICE1, "Node1"),
  ok = seagull:deregister(?TEST_SERVICE1, "Node2"),
  ok = seagull:deregister(?TEST_SERVICE2, "Node1"),

  undefined = seagull:get_service(?TEST_SERVICE1),
  undefined = seagull:get_service(?TEST_SERVICE2),

  Config.

test_get_services(Config) ->
  start_consul(),

  ok = seagull:register(?TEST_SERVICE1, "127.0.0.1", 4232, "Node1"),
  ok = seagull:register(?TEST_SERVICE2, "127.0.0.1", 4232, "Node1"),
  ok = seagull:register(?TEST_SERVICE2, "127.0.0.1", 5131, "Node2"),

  {ok, Services} = seagull:get_services(),
  ?assertEqual(#{<<"consul">> => [], <<"test_service1">> => [], <<"test_service2">> => []}, Services),

  ok = seagull:deregister(?TEST_SERVICE2, "Node1"), % service 2 still lives on Node2
  {ok, Services2} = seagull:get_services(),
  ?assertEqual(#{<<"consul">> => [], <<"test_service1">> => [], <<"test_service2">> => []}, Services2),

  ok = seagull:deregister(?TEST_SERVICE2, "Node2"), % service 2 is out
  {ok, Services3} = seagull:get_services(),
  ?assertEqual(#{<<"consul">> => [], <<"test_service1">> => []}, Services3),

  ok = seagull:deregister(?TEST_SERVICE1, "Node1"), % service 1 is out
  {ok, Services4} = seagull:get_services(),
  ?assertEqual(#{<<"consul">> => []}, Services4),

  Config.

test_get_services_near(Config) ->
  start_consul(),

  ok = seagull:register(?TEST_SERVICE1, "127.0.0.1", 4232),
  ok = seagull:register(?TEST_SERVICE2, "127.0.0.1", 5131, "Node2"),

  {ok, Node} = inet:gethostname(),
  NodeBin = list_to_binary(Node),
  {ok, #{<<"Node">> := NodeBin}, []} = seagull:get_service_near(?TEST_SERVICE1),

  ok = seagull:deregister(?TEST_SERVICE1),
  ok = seagull:deregister(?TEST_SERVICE2, "Node2"),

  Config.

test_get_dns_request(Config) ->
  start_consul(),

  ok = seagull:register(?TEST_SERVICE1, "127.0.0.1", 4232),
  ok = seagull:register(?TEST_SERVICE1, "127.0.0.1", 4232, "Node1"),
  ok = seagull:register(?TEST_SERVICE2, "127.0.0.1", 5131),

  Services = seagull:dns_request(?TEST_SERVICE1),
  {ok, Node} = inet:gethostname(),

  ?assert(lists:member({1, 1, 4232, "Node1.node.dc1.consul"}, Services)),
  ?assert(lists:member({1, 1, 4232, Node ++ ".node.dc1.consul"}, Services)),
  ?assert(length(Services) == 2),

  ok = seagull:deregister(?TEST_SERVICE1, "Node1"),
  ok = seagull:deregister(?TEST_SERVICE1),
  ok = seagull:deregister(?TEST_SERVICE2),

  Config.

test_set_kv(Config) ->
  start_consul(),

  Key = <<"key1">>,
  Value = <<"value1">>,

  ok = seagull:set_value(Key, Value),
  Value = seagull:get_value(Key),

  ok = seagull:set_value(Key, <<"other_value">>),
  <<"other_value">> = seagull:get_value(Key),

  ok = seagull:drop_value(Key),

  Config.

test_get_kv(Config) ->
  start_consul(),

  Key = <<"key1">>,
  Value = <<"value1">>,

  undefined = seagull:get_value(Key),

  ok = seagull:set_value(Key, Value),
  Value = seagull:get_value(Key),

  ok = seagull:drop_value(Key),
  undefined = seagull:get_value(Key),

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
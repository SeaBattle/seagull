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
    test_get_kv,
    insert_map
  ].

init_per_suite(Config) ->
  application:ensure_all_started(inets),
  true = ensure_consul(),
  Config.

end_per_suite(_Config) ->
  ok.

init_per_testcase(_Case, Config) ->
  Config.

end_per_testcase(_Case, Config) ->
  Config.

%% Tests
test_register(Config) ->
  {ok, _} = sc_conf_holder:start_link(),
  ?assert(sc_conf_holder:register_backend(sc_backend_consul, ?CONSUL_URL, [])),

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
  {ok, _} = sc_conf_holder:start_link(),
  ?assert(sc_conf_holder:register_backend(sc_backend_consul, ?CONSUL_URL, [])),

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
  {ok, Service2} = seaconfig:get_service(?TEST_SERVICE1),

  [#{<<"Address">> := <<"127.0.0.1">>,
    <<"Node">> := <<"Node1">>,
    <<"ServiceAddress">> := <<"127.0.0.1">>,
    <<"ServiceID">> := ?TEST_SERVICE1,
    <<"ServiceName">> := ?TEST_SERVICE1,
    <<"ServicePort">> := 4232},
    #{<<"Address">> := <<"127.0.0.1">>,
      <<"Node">> := <<"Node2">>,
      <<"ServiceAddress">> := <<"127.0.0.1">>,
      <<"ServiceID">> := ?TEST_SERVICE1,
      <<"ServiceName">> := ?TEST_SERVICE1,
      <<"ServicePort">> := 4232}] = Service2,

  ok = seaconfig:deregister(?TEST_SERVICE1, "Node1"),
  ok = seaconfig:deregister(?TEST_SERVICE1, "Node2"),
  ok = seaconfig:deregister(?TEST_SERVICE2, "Node1"),

  Config.

test_get_services(Config) ->
  Config.

test_get_services_near(Config) ->
  Config.

test_get_dns_request(Config) ->
  Config.

test_set_kv(Config) ->
  Config.

test_get_kv(Config) ->
  Config.

insert_map(Config) ->
  Config.


%% @private
ensure_consul() ->
%%  case httpc:request(get, {?CONSUL_URL, []}, [], []) of
%%    {ok, {{_, 200, _}, _, _}} ->
%%      true;
%%    Err ->
%%      {false, Err}
%%  end.
  true.
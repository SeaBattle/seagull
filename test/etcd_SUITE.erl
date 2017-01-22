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

all() ->
  [
    test_register,
    test_get_service,
    test_get_services,
    test_set_kv,
    test_get_kv,
    insert_map
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
test_register(Config) ->
  Config.

test_get_service(Config) ->
  Config.

test_get_services(Config) ->
  Config.

test_set_kv(Config) ->
  Config.

test_get_kv(Config) ->
  Config.

insert_map(Config) ->
  Config.

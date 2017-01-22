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


all() ->
  [
    test_auto_register,
    test_auto_update,
    test_caching_on_get,
    test_update_on_set
  ].

init_per_suite(Config) ->
  Config.

end_per_suite(_Config) ->
  ok.

init_per_testcase(_Case, Config) ->
  Config.

end_per_testcase(_Case, Config) ->
  Config.

%% Tests
test_auto_register(Config) ->
  Config.

test_auto_update(Config) ->
  Config.

test_caching_on_get(Config) ->
  Config.

test_update_on_set(Config) ->
  Config.


%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Oct 2016 18:14
%%%-------------------------------------------------------------------
-module(sc_backend_man).
-author("tihon").

%% API
-export([register_backend/3, request_backend/1, request_backend/2]).

-spec register_backend(module(), string(), proplists:proplist()) -> boolean().
register_backend(Module, Url, Opts) ->
  sc_conf_holder:register_backend(Module, Url, Opts).

-spec request_backend(atom()) -> any().
request_backend(Request) ->
  case sc_conf_holder:get_service_conf() of
    undefined -> throw(no_conf);
    {Module, Url, _} -> Module:Request(Url)
  end.

-spec request_backend(atom(), list()) -> any().
request_backend(get_value, [Key]) ->
  may_be_get_value_from_cache(Key);
request_backend(set_value, [Key, Value]) ->
  may_be_add_value_to_cache(Key, Value);
request_backend(Request, Arguments) ->
  case sc_conf_holder:get_service_conf() of
    undefined -> throw(no_conf);
    {Module, Url, _} -> call(Module, Request, [Url | Arguments])
  end.


%% @private
may_be_get_value_from_cache(Key) ->
  case sc_conf_holder:get_service_conf() of
    undefined -> throw(no_conf);
    {_, _, #{enable := true}} ->  %cache is on
      sc_conf_holder:get_value_from_cache(Key);
    {Module, Url, _} ->
      call(Module, get_value, [Url | [Key]])
  end.

%% @private
may_be_add_value_to_cache(Key, Value) ->
  case sc_conf_holder:get_service_conf() of
    undefined -> throw(no_conf);
    {Module, Url, #{enable := true}} ->  %cache is on
      case call(Module, set_value, [Url | [Key, Value]]) of
        ok ->
          sc_conf_holder:update_value_in_cache(Key, Value),
          ok;
        Err -> Err
      end;
    {Module, Url, _} ->
      call(Module, set_value, [Url | [Key, Value]])
  end.

%% @private
call(Module, Function, Args) ->
  erlang:apply(Module, Function, Args).
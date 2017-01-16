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
    {Module, Url} -> Module:Request(Url)
  end.

-spec request_backend(atom(), list()) -> any().
request_backend(Request, Arguments) ->
  case sc_conf_holder:get_service_conf() of
    undefined -> throw(no_conf);
    {Module, Url} -> erlang:apply(Module, Request, [Url | Arguments])
  end.
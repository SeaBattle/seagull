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
-export([prepare_backend/1, request_backend/1, request_backend/2]).

-spec prepare_backend(tuple()) -> tuple().
prepare_backend({Backend, BackendUrl}) when is_atom(Backend) ->
  Module = case Backend of
             consul -> sc_backend_consul;
             etcd -> sc_backend_etcd
           end,
  {Module, BackendUrl}.

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
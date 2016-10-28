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
-export([prepare_backend/1]).

-spec prepare_backend(tuple()) -> tuple().
prepare_backend({Backend, BackendUrl}) when is_atom(Backend) ->
  Module = case Backend of
             consul -> sc_backend_consul;
             etcd -> sc_backend_etcd
           end,
  {Module, BackendUrl}.
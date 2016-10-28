%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Oct 2016 18:15
%%%-------------------------------------------------------------------
-module(sc_backend_consul).
-author("tihon").

-behavior(sc_backend).

%% API
-export([get_service/2, get_services/1]).

-define(SERVICES, "~s/").
-define(SERVICE, "~s/").

get_service(Host, Name) ->
  erlang:error(not_implemented).

get_services(Host) ->
  erlang:error(not_implemented).
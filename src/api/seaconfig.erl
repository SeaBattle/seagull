%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Oct 2016 20:12
%%%-------------------------------------------------------------------
-module(seaconfig).
-author("tihon").

%% API
-export([add_callback/2, remove_callback/1]).

-spec add_callback(binary(), fun()) -> true.
add_callback(Var, Fun) when is_binary(Var), is_function(Fun) -> true.

-spec remove_callback(binary()) -> ok.
remove_callback(Var) -> ok.
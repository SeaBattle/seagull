%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Oct 2016 18:14
%%%-------------------------------------------------------------------
-module(sc_backend).
-author("tihon").

-callback get_service(Host :: string(), Name :: string()) -> {ok, map()} | {error, any()}.

-callback get_services(Host :: string()) -> {ok, map()} | {error, any()}.

%% Can be called manually and in sc_conf_holder:check_auto_register/2 via providing autoregister option in sys.config
-callback register(Host :: string(), Service :: string(), LocalAddress :: string(), Port :: integer()) ->
  ok | {error, any()}.

-callback get_value(Host :: string(), Key :: binary()) -> Value :: binary() | undefined | {error, any()}.

-callback set_value(Host :: string(), Key :: binary(), Value :: binary()) -> ok | {error, any()}.
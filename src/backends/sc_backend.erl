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

-callback get_service(Host :: string(), Name :: binary()) -> list().

-callback get_services(Host) -> list().
%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(sc_conf_holder).
-author("tihon").

-behaviour(gen_server).

-include("sc_headers.hrl").

%% API
-export([start_link/0, get_service_conf/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(CONF_ETS, sc_conf).

-record(state, {url :: string(), module :: atom()}).

%%%===================================================================
%%% API
%%%===================================================================
-spec get_service_conf() -> {atom(), string()} | undefined.
get_service_conf() ->
  case ets:lookup(?CONF_ETS, conf) of
    [{_, Module, Url}] -> {Module, Url};
    _ -> undefined
  end.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  ets:new(?CONF_ETS, [named_table, protected, {read_concurrency, true}, {write_concurrency, true}]),
  {ok, Backend} = application:get_env(seaconfig, backend),
  {Module, BackendUrl} = sc_backend_man:prepare_backend(Backend),
  ets:insert(?CONF_ETS, {conf, Module, BackendUrl}),
  check_auto_register(Module, BackendUrl),
  {ok, #state{url = BackendUrl, module = Module}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @private
check_auto_register(Module, BackendUrl) ->
  case application:get_env(seaconfig, autoregister, false) of
    false -> ok;
    #{service := Service, address := Address, port := Port} ->
      Module:register(BackendUrl, Service, Address, Port)
  end.
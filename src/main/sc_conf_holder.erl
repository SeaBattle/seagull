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

%% API
-export([start_link/0, get_service_conf/0, register_backend/3]).

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

register_backend(Module, Url, Opts) ->
  gen_server:call(?MODULE, {backend, Module, Url, Opts}).

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
  Register = application:get_env(seaconfig, autoregister, false),
  Backend = application:get_env(seaconfig, backend),
  {_, State} = do_register_backend(Backend, Register),
  {ok, State}.



handle_call({backend, Module, Url, Opts}, _From, _) ->
  Register = proplists:get_value(autoregister, Opts, false),
  {Res, UState} = do_register_backend({ok, {Module, Url}}, Register),
  {reply, Res, UState};
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
do_register_backend(undefined, _) ->
  {false, #state{}};
do_register_backend({ok, Backend}, Autoregister) ->
  {Module, BackendUrl} = prepare_backend(Backend),
  ets:insert(?CONF_ETS, {conf, Module, BackendUrl}),
  check_auto_register(Module, BackendUrl, Autoregister),
  {true, #state{url = BackendUrl, module = Module}}.

%% @private
check_auto_register(_, _, false) -> ok;
check_auto_register(Module, BackendUrl, #{service := Service, address := Address, port := Port}) ->
  Module:register(BackendUrl, Service, Address, Port).

%% @private
-spec prepare_backend(tuple()) -> tuple().
prepare_backend({Backend, BackendUrl}) when is_atom(Backend) ->
  Module = case Backend of
             consul -> sc_backend_consul;
             etcd -> sc_backend_etcd
           end,
  {Module, BackendUrl}.
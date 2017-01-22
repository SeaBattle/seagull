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
-export([start_link/0,
  get_service_conf/0,
  register_backend/3,
  get_value_from_cache/1,
  update_value_in_cache/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_UPDATE_TIME, 5000).   %5 sec
-define(CONF_ETS, sc_conf).

-record(state,
{
  url :: string(),
  module :: atom(),
  upd_time :: tuple() %cache sync int time + timer ref
}).

%%%===================================================================
%%% API
%%%===================================================================
-spec get_service_conf() -> {atom(), string(), map()} | undefined.
get_service_conf() ->
  case ets:lookup(?CONF_ETS, conf) of
    [{_, Module, Url, Caching}] -> {Module, Url, Caching};
    _ -> undefined
  end.

%% Try get value from cache. If not found - request backend and save result
%% to cache.
-spec get_value_from_cache(binary()) -> undefined | binary() | {error, any()}.
get_value_from_cache(Key) ->
  case ets:lookup(?CONF_ETS, Key) of
    [] -> %miss cache
      {Module, Url, _} = get_service_conf(),
      update_value(Key, Module, Url);
    Results ->
      [{_, V}] = lists:filter(fun(Tuple) -> tuple_size(Tuple) == 2 end, Results),
      V
  end.

%% Add new or update existing value in cache.
-spec update_value_in_cache(binary(), _Value :: binary() | undefined) -> _Value.
update_value_in_cache(Key, Value) ->
  do_update_value(Key, Value).

-spec register_backend(module(), string(), proplists:proplist()) -> boolean().
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
  Caching = application:get_env(seaconfig, cache, #{enable => false}),
  Backend = application:get_env(seaconfig, backend),
  UpdTime = cache_init_update(Caching),
  {_, State} = do_register_backend(Backend, Register, Caching),
  {ok, State#state{upd_time = UpdTime}}.



handle_call({backend, Module, Url, Opts}, _From, _) ->
  Register = proplists:get_value(autoregister, Opts, false),
  Caching = proplists:get_value(cache, Opts, #{enable => false}),
  {Res, UState} = do_register_backend({ok, {Module, Url}}, Register, Caching),
  {reply, Res, UState};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(update, State = #state{upd_time = {Time, Ref}, module = Module, url = Url}) ->
  erlang:cancel_timer(Ref),
  List = ets:tab2list(?CONF_ETS),
  lists:foreach(fun(Key) -> update_value(Key, Module, Url) end, List),
  Tref = erlang:send_after(Time, self(), update),
  {noreply, State#state{upd_time = {Time, Tref}}};
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
do_register_backend(undefined, _, _) ->
  {false, #state{}};
do_register_backend({ok, {Module, BackendUrl}}, Autoregister, Caching) ->
  ets:insert(?CONF_ETS, {conf, Module, BackendUrl, Caching}),
  check_auto_register(Module, BackendUrl, Autoregister),
  {true, #state{url = BackendUrl, module = Module}}.

%% @private
cache_init_update(#{enable := false}) -> undefined;
cache_init_update(CacheConf = #{enable := true}) ->
  UpdTime = maps:get(update_time, CacheConf, ?DEFAULT_UPDATE_TIME),
  case UpdTime of
    undefined -> undefined;
    UpdTime ->
      TRef = erlang:send_after(UpdTime, self(), update),
      {UpdTime, TRef}
  end.

%% @private
check_auto_register(_, _, false) -> ok;
check_auto_register(Module, BackendUrl, #{service := Service, address := Address, port := Port}) ->
  Module:register(BackendUrl, Service, Address, Port).

%% @private
update_value(Key, Module, Url) ->
  case Module:get_value(Url, Key) of
    {error, Err} -> {error, Err};
    AnyRes ->   %cache result from kv. Can be real value or undefined
      do_update_value(Key, AnyRes)
  end.

%% @private
do_update_value(Key, Value) ->
  ets:insert(?CONF_ETS, {Key, Value}),
  Value.
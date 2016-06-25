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
-export([start_link/0, get_conf/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(CONF_ETS, ss_conf).
-define(UPDATE_CONF_INTERVAL, 900000). %15 minutes

-record(state, {url}).

%%%===================================================================
%%% API
%%%===================================================================
-spec get_conf(binary(), any()) -> any().
get_conf(Var, Default) ->
  case ets:lookup(?CONF_ETS, Var) of
    [] -> Default;
    [{_, Res}] -> Res
  end.

-spec get_conf(binary()) -> binary().
get_conf(Var) ->
  get_conf(Var, undefined).

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
  {ok, Url} = application:get_env(seaconfig, etcd_url),
  true = load_conf(Url),
  erlang:send_after(?UPDATE_CONF_INTERVAL, self(), update_conf),
  {ok, #state{url = Url}}.


handle_call(_Request, _From, State) ->
  {reply, ok, State}.


handle_cast(_Request, State) ->
  {noreply, State}.


handle_info(update_conf, State = #state{url = Url}) ->  %update configuration
  _ = load_conf(Url),
  erlang:send_after(?UPDATE_CONF_INTERVAL, self(), update_conf),
  {noreply, State, hibernate};
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
load_conf(Url) ->
  case httpc:request(get, {Url, []}, [], []) of
    {ok, {{_, 200, _}, _, Res}} ->
      Decoded = jsone:decode(list_to_binary(Res), [{object_format, map}]),
      save_conf(Decoded),
      true;
    Reason ->
      timer:sleep(1000),  %sleep before crash so it can be restarted via supervisor on start
      {false, Reason}
  end.

%% @private
save_conf(#{<<"node">> := #{<<"nodes">> := Conf}}) ->
  lists:foreach(fun parse_and_save/1, Conf).

%% @private
parse_and_save(#{<<"key">> := <<"/config/">>, <<"nodes">> := Conf}) ->
  lists:foreach(fun do_save_conf/1, Conf).

%% @private
do_save_conf(#{<<"key">> := <<"/config/", Key/binary>>, <<"value">> := Value}) ->
  ets:insert(?CONF_ETS, {Key, Value});
do_save_conf(#{<<"key">> := <<"/config/", _/binary>>, <<"nodes">> := Conf}) ->
  lists:foreach(fun save_conf/1, Conf).
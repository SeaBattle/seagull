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
-export([start_link/0, get_conf/1, set_conf/2, get_conf/2, set_inorder_conf/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(CONF_ETS, ss_conf).
-define(INORDER_KEYS, [?USER_SERVICE_HOSTS]).
-define(UPDATE_CONF_INTERVAL, 900000). %15 minutes
-define(QUERY_SERVICES_INTERVAL, 500).  %0.5 sec for query services

-record(state, {url :: string()}).

%%%===================================================================
%%% API
%%%===================================================================
-spec get_conf(binary(), any()) -> any().
get_conf(Var, Default) ->
  case ets:lookup(?CONF_ETS, Var) of
    [] -> Default;
    [{_, Res}] -> Res;  %ordinary key
    Res = [_ | _] -> lists:map(fun({_, G, R}) -> {G, R} end, Res)  %generated keys
  end.

-spec get_conf(binary()) -> binary().
get_conf(Var) ->
  get_conf(Var, undefined).

-spec set_conf(binary(), binary()) -> true | {false, any()}.
set_conf(Key, Value) ->
  gen_server:call(?MODULE, {set, Key, Value}).

-spec set_inorder_conf(binary(), binary()) -> true | {false, any()}.
set_inorder_conf(Key, Value) ->
  gen_server:call(?MODULE, {set_inorder, Key, Value}).

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


handle_call({set, Key, Value}, _From, State = #state{url = Url}) ->
  set_conf(Url, Key, Value),
  {reply, ok, State};
handle_call({set_inorder, Key, Value}, _From, State = #state{url = Url}) ->
  set_inorder_conf(Url, Key, Value),
  {reply, ok, State};
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
  case httpc:request(get, {Url ++ "?recursive=true", []}, [], []) of
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
  case lists:member(Key, ?INORDER_KEYS) of
    true -> %split key to fixed and generated part
      Splitted = binary:split(Key, <<"/">>, [global]),
      End = lists:last(Splitted),
      UKey = binary:replace(Key, <<<<"/">>/binary, End/binary>>, <<"">>),
      ets:insert(?CONF_ETS, {UKey, Splitted, Value});
    false ->  %normal key, just save it
      ets:insert(?CONF_ETS, {Key, Value})
  end;
do_save_conf(#{<<"key">> := <<"/config/", _/binary>>, <<"nodes">> := Conf}) ->
  lists:foreach(fun save_conf/1, Conf).

%% @private
set_conf(Url, Key, Value) ->  %TODO testme!
  case httpc:request(put, {Url ++ binary_to_list(Key), <<"text/html">>, [], Value}, [], []) of
    {ok, {{_, 200, _}, _, _Res}} ->  %TODO match res
      ets:insert(?CONF_ETS, {Key, Value}),
      true;
    Reason ->
      {false, Reason}
  end.

%% @private
set_inorder_conf(Url, Key, Value) ->  %TODO testme!
  FullUrl = Url ++ binary_to_list(Key),
  case httpc:request(post, {FullUrl, <<"text/html">>, [], Value}, [], []) of
    {ok, {{_, 200, _}, _, _Res}} ->  %TODO match res
      save_inroder_conf(FullUrl);
    Reason ->
      {false, Reason}
  end.

%% @private
save_inroder_conf(FullUrl) ->  %TODO testme!
  case httpc:request(get, {FullUrl, []}, [], []) of
    {ok, {{_, 200, _}, _, Res}} ->
      #{<<"node">> := Decoded} = jsone:decode(list_to_binary(Res), [{object_format, map}]),
      parse_and_save(Decoded),
      true;
    Reason ->
      {false, Reason}
  end.
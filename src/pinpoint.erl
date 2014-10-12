-module(pinpoint).

-behaviour(gen_server).

%% API
-export([
  start_link/1,
  insert/2,
  search/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
  tid :: atom(),
  data_set :: atom()
}).

-callback indexed_fields() -> [atom()].

%%%===================================================================
%%% API
%%%===================================================================

start_link(DataSet) ->
  gen_server:start_link({local, DataSet}, ?MODULE, [DataSet], []).

insert(DataSet, Data) when is_map(Data) ->
  BlownUpData = blow_up(DataSet, Data),
  ets:insert(DataSet, [BlownUpData]).

search(DataSet, Query) ->
  MappedQuery = mapped_query(DataSet, Query),
  {Exact, Condition} = maps:fold(fun
    (Field, {Operator, Value}, {ExactMatch, ConditionMatch}) -> {ExactMatch, [{Operator, Field, Value} | ConditionMatch]};
    (Field, Value, {ExactMatch, ConditionMatch}) -> {replace_match(ExactMatch, Field, Value), ConditionMatch}
  end, {index_query_line(DataSet), []}, MappedQuery),

  CompleteMatch = [{list_to_tuple(Exact), Condition, ['$_']}],
  Data = ets:select(DataSet, CompleteMatch),
  {CompleteMatch, [combine(DataSet, D) || D <- Data]}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([DataSet]) ->
  Tid = ets:new(DataSet, [{read_concurrency, true}, {write_concurrency, true}, named_table, public]),
  {ok, #state{tid = Tid, data_set = DataSet}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Blows the data up into a tuple where the first element is a timestamp of now(),
%% the second is a map of non-indexed values, and the remaining elements are indexed values
blow_up(DataSet, Data) ->
  Indices = DataSet:indexed_fields(),
  IndexedValues = [maps:get(Field, Data, undefined) || Field <- Indices],
  CleanedData = maps:without(Indices, Data),
  list_to_tuple([timestamp_now(), CleanedData] ++ IndexedValues).

%% Returns the tuple index of the field in the blown up data set. This is
%% always 2 greater than the position of the field in the indices because
%% the blown up data contains a map and a timestamp.
index_of(DataSet, Field) ->
  Indices = DataSet:indexed_fields(),
  IndexFind = fun
    IndexFind([], _) -> -1;
    IndexFind([K | _], Pos) when K == Field -> Pos;
    IndexFind([_ | Rest], Pos) -> IndexFind(Rest, Pos + 1)
  end,
  % Offset by 3 for 1-indexed tuples and 2 padding elements
  list_to_atom("$" ++ integer_to_list(IndexFind(Indices, 3))).

index_query_line(DataSet) ->
  Indices = DataSet:indexed_fields(),
  [list_to_atom("$" ++ integer_to_list(I)) || I <- lists:seq(1, length(Indices) + 2)].

mapped_query(DataSet, Query) ->
  maps:fold(fun(K, V, Response) -> maps:put(index_of(DataSet, K), V, Response) end, #{}, Query).

%% Combines blown_up data by reversing the encoding done by blow_up/2.
combine(DataSet, Data) ->
  Indices = DataSet:indexed_fields(),
  [_, MapData | ListData] = tuple_to_list(Data),
  IndexedData = maps:from_list(lists:zip(Indices, ListData)),
  maps:merge(MapData, IndexedData).

timestamp_now() ->
  {Mega, Sec, Micro} = erlang:now(),
  Mega * 1000 * 1000 + Sec * 1000 + Micro.

replace_match(KeyMatches, Key, Value) ->
  lists:map(fun
    (K) when K == Key -> Value;
    (K) -> K
  end, KeyMatches).


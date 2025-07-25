-module(pollution_value_collector_gen_statem).
-author("paranoidppl").

-behaviour(gen_statem).

%% API
-export([start_link/0, stop/0]). % Added stop/0
-export([set_station/1, add_value/2, add_value/3, store_data/0]).

-export([init/1, terminate/3, code_change/4, handle_event/4, callback_mode/0]). % Added code_change and handle_event

-define(SERVER, ?MODULE).

% Internal record for the collector's state
-record(collector_state, {
    station_id :: atom() | string() | {float(), float()} | none,
    pending_readings = [] :: list({DateTime :: term(), Type :: atom(), Value :: number()})
}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_statem:cast(?SERVER, stop).

set_station(StationId) ->
    gen_statem:cast(?SERVER, {set_station, StationId}).

add_value(ReadingType, ReadingValue) ->
    CurrentDateTime = calendar:local_time(),
    add_value(ReadingType, ReadingValue, CurrentDateTime).

add_value(ReadingType, ReadingValue, DateTime) ->
    gen_statem:cast(?SERVER, {add_value, ReadingType, ReadingValue, DateTime}).

store_data() ->
    gen_statem:cast(?SERVER, store_data).

callback_mode() ->
    handle_event_function.

init([]) ->
    io:format("~p: init~n", [?MODULE]),
    InitialStateData = #collector_state{station_id = none, pending_readings = []},
    {ok, waiting_for_station, InitialStateData}.

handle_event(cast, {set_station, StationId}, waiting_for_station, Data = #collector_state{}) ->
    io:format("~p: cast {set_station, ~p} in state waiting_for_station~n", [?MODULE, StationId]),
    {next_state, collecting_values, Data#collector_state{station_id = StationId}};

handle_event(cast, {add_value, _ReadingType, _ReadingValue, _DateTime}, waiting_for_station, _Data) ->
    io:format("~p: Error - add_value called before station is set.~n", [?MODULE]),
    {keep_state_and_data};

handle_event(cast, store_data, waiting_for_station, _Data) ->
    io:format("~p: Error - store_data called before station is set or no data to store.~n", [?MODULE]),
    {keep_state_and_data};

handle_event(cast, {set_station, _NewStationId}, collecting_values, _Data) ->
    io:format("~p: Error - set_station called while already collecting for station ~p.~n", [?MODULE, _Data#collector_state.station_id]),
    {keep_state_and_data};

handle_event(cast, {add_value, ReadingType, ReadingValue, DateTime}, collecting_values, Data = #collector_state{pending_readings = Readings}) ->
    io:format("~p: cast {add_value, ~p, ~p, ~p} in state collecting_values~n", [?MODULE, ReadingType, ReadingValue, DateTime]),
    NewReading = {DateTime, ReadingType, ReadingValue},
    UpdatedReadings = [NewReading | Readings],
    {keep_state, Data#collector_state{pending_readings = UpdatedReadings}};

handle_event(cast, store_data, collecting_values, Data = #collector_state{station_id = StationId, pending_readings = ReadingsToStore}) ->
    io:format("~p: cast store_data for station ~p with ~p readings~n", [?MODULE, StationId, length(ReadingsToStore)]),
    if
        StationId == none ->
            io:format("~p: Error - store_data called but station_id is 'none'. This should not happen in 'collecting_values' state.~n", [?MODULE]);
        ReadingsToStore == [] ->
            io:format("~p: No new readings to store for station ~p.~n", [?MODULE, StationId]);
        true ->
            lists:foreach(
                fun({DT, T, V}) ->
                    pollution_gen_server:add_value(StationId, DT, T, V)
                end,
                lists:reverse(ReadingsToStore)
            )
    end,
    {next_state, waiting_for_station, #collector_state{station_id = none, pending_readings = []}};

handle_event(cast, stop, _AnyState, _Data) ->
    io:format("~p: Received stop cast.~n", [?MODULE]),
    {stop, normal};

handle_event(cast, EventContent, StateName, _Data) ->
    io:format("~p: Unhandled cast ~p in state ~p~n", [?MODULE, EventContent, StateName]),
    {keep_state_and_data};

handle_event(EventType, EventContent, StateName, _Data) ->
    io:format("~p: Unhandled event type ~p with content ~p in state ~p~n", [?MODULE, EventType, EventContent, StateName]),
    {keep_state_and_data}.


terminate(Reason, StateName, StateData) ->
    io:format("~p: terminating. Reason: ~p. In state: ~p. With data: ~p~n",
        [?MODULE, Reason, StateName, StateData]),
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

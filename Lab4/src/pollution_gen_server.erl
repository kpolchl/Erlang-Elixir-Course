%%%-------------------------------------------------------------------
%%% @author your_name
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%   Pollution data management gen_server.
%%% @end
%%% Created : 02. Apr 2025
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([
    add_station/2,
    add_value/4,
    remove_value/3,
    get_one_value/3,
    get_station_mean/2,
    get_daily_mean/2,
    get_daily_average_data_count/1,
    get_station_min/2,
    crash/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE). % For local registration

%%% Records (copied from pollution.erl for convenience, or could be in a .hrl file)
-record(monitor, {name_map = #{}, coordinate_map = #{}}).
-record(station, {name, coordinate, readings = []}).
-record(reading, {type, value, datetime}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).

add_station(Name, Coord) ->
    gen_server:call(?SERVER, {add_station, Name, Coord}).

add_value(StationId, DateTime, Type, Value) ->
    gen_server:call(?SERVER, {add_value, StationId, DateTime, Type, Value}).

remove_value(StationId, DateTime, Type) ->
    gen_server:call(?SERVER, {remove_value, StationId, DateTime, Type}).

get_one_value(StationId, DateTime, Type) ->
    gen_server:call(?SERVER, {get_one_value, StationId, DateTime, Type}).

get_station_mean(StationId, Type) ->
    gen_server:call(?SERVER, {get_station_mean, StationId, Type}).

get_daily_mean(ReadingType, Date) ->
    gen_server:call(?SERVER, {get_daily_mean, ReadingType, Date}).

get_daily_average_data_count(Date) ->
    gen_server:call(?SERVER, {get_daily_average_data_count, Date}).

get_station_min(StationId, Type) ->
    gen_server:call(?SERVER, {get_station_min, StationId , Type}).

crash() ->
    gen_server:cast(?SERVER, crash). % Using cast as we don't expect a reply from a crash

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    io:format("pollution_gen_server: init~n", []),
    Monitor = pollution:create_monitor(), % Use the original module to create the initial state
    {ok, Monitor}.

handle_call({add_station, Name, Coord}, _From, Monitor) ->
    io:format("pollution_gen_server: add_station ~p ~p~n", [Name, Coord]),
    Result = pollution:add_station(Name, Coord, Monitor),
    NewMonitor = case Result of
                     #monitor{} -> Result; % If successful, Result is the new monitor state
                     {error, _} -> Monitor  % If error, state does not change
                 end,
    {reply, Result, NewMonitor};

handle_call({add_value, StationId, DateTime, Type, Value}, _From, Monitor) ->
    io:format("pollution_gen_server: add_value ~p ~p ~p ~p~n", [StationId, DateTime, Type, Value]),
    Result = pollution:add_value(StationId, DateTime, Type, Value, Monitor),
    NewMonitor = case Result of
                     #monitor{} -> Result;
                     {error, _} -> Monitor
                 end,
    {reply, Result, NewMonitor};

handle_call({remove_value, StationId, DateTime, Type}, _From, Monitor) ->
    io:format("pollution_gen_server: remove_value ~p ~p ~p~n", [StationId, DateTime, Type]),
    Result = pollution:remove_value(StationId, DateTime, Type, Monitor),
    NewMonitor = case Result of
                     #monitor{} -> Result;
                     {error, _} -> Monitor
                 end,
    {reply, Result, NewMonitor};

handle_call({get_one_value, StationId, DateTime, Type}, _From, Monitor) ->
    io:format("pollution_gen_server: get_one_value ~p ~p ~p~n", [StationId, DateTime, Type]),
    Result = pollution:get_one_value(StationId, DateTime, Type, Monitor),
    {reply, Result, Monitor}; % State does not change for 'get' operations

handle_call({get_station_mean, StationId, Type}, _From, Monitor) ->
    io:format("pollution_gen_server: get_station_mean ~p ~p~n", [StationId, Type]),
    Result = pollution:get_station_mean(StationId, Type, Monitor),
    {reply, Result, Monitor};

handle_call({get_daily_mean, ReadingType, Date}, _From, Monitor) ->
    io:format("pollution_gen_server: get_daily_mean ~p ~p~n", [ReadingType, Date]),
    Result = pollution:get_daily_mean(ReadingType, Date, Monitor),
    {reply, Result, Monitor};

handle_call({get_daily_average_data_count, Date}, _From, Monitor) ->
    io:format("pollution_gen_server: get_daily_average_data_count ~p~n", [Date]),
    Result = pollution:get_daily_average_data_count(Date, Monitor),
    {reply, Result, Monitor};

handle_call({get_station_min, StationId, Type}, _From, Monitor) ->
    io:format("pollution_gen_server: get_station_min ~p ~p~n", [StationId, Type]),
    Result = pollution:get_station_min(StationId, Type, Monitor),
    {reply, Result, Monitor};


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(Request, _From, State) ->
    io:format("pollution_gen_server: unhandled_call ~p~n", [Request]),
    {reply, {error, unhandled_call}, State}.


handle_cast(crash, State) ->
    io:format("pollution_gen_server: Crashing intentionally...~n", []),
    1 / 0, % This will cause a badarith error
    {noreply, State}; % This line will not be reached

handle_cast(Msg, State) ->
    io:format("pollution_gen_server: unhandled_cast ~p~n", [Msg]),
    {noreply, State}.


handle_info(Info, State) ->
    io:format("pollution_gen_server: unhandled_info ~p~n", [Info]),
    {noreply, State}.


terminate(Reason, _State) ->
    io:format("pollution_gen_server: terminating with reason ~p~n", [Reason]),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions (if any)
%%%===================================================================
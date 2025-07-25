%%%-------------------------------------------------------------------
%%% @author paranoidppl
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Apr 2025 23:34
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("paranoidppl").

%% API
-export([
    start/0, stop/0,
    add_station/2, add_value/4, remove_value/3,
    get_one_value/3, get_station_mean/2,
    get_daily_mean/2, get_daily_average_data_count/1
]).

start() ->
    register(server, spawn(fun() -> loop(none) end)),
    server ! init.

stop() ->
    server ! kill.

add_station(Name, Coord) ->
    server ! {add_station, {Name, Coord}}.

add_value(StationId, DateTime, Type, Value) ->
    server ! {add_value, {StationId, DateTime, Type, Value}}.

remove_value(StationId, DateTime, Type) ->
    server ! {remove_value, {StationId, DateTime, Type}}.

get_one_value(StationId, DateTime, Type) ->
    server ! {get_one_value, {StationId, DateTime, Type}, self()},
    receive
        Result -> Result
    end.

get_station_mean(StationId, Type) ->
    server ! {get_station_mean, {StationId, Type}, self()},
    receive
        Result -> Result
    end.

get_daily_mean(ReadingType, Date) ->
    server ! {get_daily_mean, {ReadingType, Date}, self()},
    receive
        Result -> Result
    end.

get_daily_average_data_count(Date) ->
    server ! {get_daily_average_data_count, {Date}, self()},
    receive
        Result -> Result
    end.

show_error(Error) ->
    io:format("Error: ~p~n", [Error]).

loop(Monitor) ->
    receive
        init ->
            case Monitor of
                none ->
                    NewMonitor = pollution:create_monitor(),
                    loop(NewMonitor);
                _ ->
                    show_error("Monitor is already initialised"),
                    loop(Monitor)
            end;

        kill -> ok;

        {add_station, {Name, Coord}} ->
            case pollution:add_station(Name, Coord, Monitor) of
                {error, Message} ->
                    show_error(Message),
                    loop(Monitor);
                NewMonitor ->
                    loop(NewMonitor)
            end;

        {add_value, {StationId, DateTime, Type, Value}} ->
            case pollution:add_value(StationId, DateTime, Type, Value, Monitor) of
                {error, Message} ->
                    show_error(Message),
                    loop(Monitor);
                NewMonitor ->
                    loop(NewMonitor)
            end;

        {remove_value, {StationId, DateTime, Type}} ->
            case pollution:remove_value(StationId, DateTime, Type, Monitor) of
                {error, Message} ->
                    show_error(Message),
                    loop(Monitor);
                NewMonitor ->
                    loop(NewMonitor)
            end;

        {get_one_value, {StationId, DateTime, Type}, PID} ->
            Result = pollution:get_one_value(StationId, DateTime, Type, Monitor),
            (case Result of
                 {error, Message} -> show_error(Message);
                 _ -> ok
             end),
            PID ! Result,
            loop(Monitor);

        {get_station_mean, {StationId, Type}, PID} ->
            Result = pollution:get_station_mean(StationId, Type, Monitor),
            (case Result of
                 {error, Message} -> show_error(Message);
                 _ -> ok
             end),
            PID ! Result,
            loop(Monitor);

        {get_daily_mean, {ReadingType, Date}, PID} ->
            Result = pollution:get_daily_mean(ReadingType, Date, Monitor),
            (case Result of
                 {error, Message} -> show_error(Message);
                 _ -> ok
             end),
            PID ! Result,
            loop(Monitor);

        {get_daily_average_data_count, {Date}, PID} ->
            Result = pollution:get_daily_average_data_count(Date, Monitor),
            (case Result of
                 {error, Message} -> show_error(Message);
                 _ -> ok
             end),
            PID ! Result,
            loop(Monitor);

        _ ->
            io:format("Command not found~n"),
            loop(Monitor)
    end.

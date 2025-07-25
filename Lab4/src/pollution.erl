%%%-------------------------------------------------------------------
%%% @author paranoidppl
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Apr 2025 21:22
%%%-------------------------------------------------------------------
-module(pollution).
-author("paranoidppl").

%%API
-export([create_monitor/0, add_station/3, add_value/5, remove_value/4,
    get_one_value/4, get_station_mean/3, get_daily_mean/3, get_daily_average_data_count/2,get_station_min/3,get_station/2]).

-record(monitor, {name_map = #{}, coordinate_map = #{}}).
-record(station, {name, coordinate, readings = []}).
-record(reading, {type, value, datetime}).


create_monitor() -> #monitor{ name_map = #{} , coordinate_map = #{}}.

get_station(StationId, #monitor{name_map = NameMap, coordinate_map = CoordMap}) ->
    case StationId of
        {_, _} -> maps:get(StationId, CoordMap, {error,no_station_coord});
        _ -> maps:get(StationId, NameMap, {error,no_station_name})
    end.


add_station(Name, Coord, Monitor = #monitor{name_map = NameMap, coordinate_map = CoordMap}) ->
    case {maps:is_key(Name, NameMap), maps:is_key(Coord, CoordMap)} of
        {true, _} -> {error, station_name_exists};
        {_, true} -> {error, station_coordinate_exists};
        _ ->
            Station = #station{name = Name, coordinate = Coord},
            Monitor#monitor{
                name_map = NameMap#{Name => Station},
                coordinate_map = CoordMap#{Coord => Station}
            }
    end.


update_station(Station, Monitor) ->
    #station{name = Name, coordinate = Coord} = Station,
    Monitor#monitor{
        name_map = maps:put(Name, Station, Monitor#monitor.name_map),
        coordinate_map = maps:put(Coord, Station, Monitor#monitor.coordinate_map)
    }.


add_value(StationId, DateTime, Type, Value, Monitor) ->
    case get_station(StationId, Monitor) of
        {error,_} -> {error, no_station};
        Station = #station{readings = Readings} ->
            case lists:any(fun(#reading{type = T, datetime = DT}) ->
                T == Type andalso DT == DateTime
                           end, Readings) of
                true -> {error, reading_not_unique};
                false ->
                    NewReading = #reading{type = Type, value = Value, datetime = DateTime},
                    update_station(Station#station{
                        readings = [NewReading | Readings]
                    }, Monitor)
            end
    end.

remove_value(StationId, DateTime, Type, Monitor) ->
    case get_station(StationId, Monitor) of
        {error,_} -> {error, no_station};
        Station = #station{readings = Readings} ->
            NewReadings = lists:filter(fun(#reading{type = T, datetime = DT}) ->
                not (T == Type andalso DT == DateTime)
                                       end, Readings),
            case length(Readings) == length(NewReadings) of
                true -> {error, no_such_reading};
                false -> update_station(Station#station{readings = NewReadings}, Monitor)
            end
    end.

get_one_value(StationId, DateTime, Type, Monitor) ->
    case get_station(StationId, Monitor) of
        {error,_} -> {error, no_station};
        #station{readings = Readings} ->
            case [V || #reading{type = T, datetime = DT, value = V} <- Readings,
                T == Type, DT == DateTime] of
                [Value] -> Value;
                [] -> {error, no_matching_values};
                _ -> {error, too_many_readings}
            end
    end.

get_station_mean(StationId, Type, Monitor) ->
    case get_station(StationId, Monitor) of
        {error,_} -> {error, no_station};
        #station{readings = Readings} ->
            Values = [V || #reading{type = T, value = V} <- Readings, T == Type],
            case Values of
                [] -> {error, no_readings};
                _ -> lists:sum(Values) / length(Values)
            end
    end.

get_daily_mean(ReadingType, Date, Monitor) ->
    AllReadings = lists:flatmap(fun(S) -> S#station.readings end,
        maps:values(Monitor#monitor.name_map)),

    DayReadings = [R#reading.value || R <- AllReadings, R#reading.type == ReadingType, element(1, R#reading.datetime) == Date],
    case DayReadings of
        [] -> {error, no_such_readings_this_day};
        _ ->
            Sum = lists:sum(DayReadings),
            Sum / length(DayReadings)
    end.

get_daily_average_data_count(Date,Monitor) ->
    NumofStations = length(maps:values(Monitor#monitor.name_map)),
    AllReadings = lists:flatmap(fun(S) -> S#station.readings end,
        maps:values(Monitor#monitor.name_map)),
    DayReadings = [R#reading.value || R <- AllReadings, element(1, R#reading.datetime) == Date],
    length(DayReadings) / NumofStations.

get_station_min(StationId, Type, Monitor) ->
    case get_station(StationId, Monitor) of
        {error,_} -> {error, no_station};
        #station{readings = Readings} ->
            Values = [V || #reading{type = T, value = V} <- Readings, T == Type],
            case Values of
                [] -> {error, no_readings};
                _ -> lists:min(Values)
            end
    end.

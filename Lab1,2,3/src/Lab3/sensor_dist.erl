%%%-------------------------------------------------------------------
%%% @author paranoidppl
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Apr 2025 14:25
%%%-------------------------------------------------------------------
-module(sensor_dist).
-author("paranoidppl").

%% API
-export([get_rand_locations/1, find_for_person/2, find_closest/2, find_closest_parallel/2 , compare_speeds/2]).

get_rand_locations(N) ->
    [{rand:uniform(10000),rand:uniform(10000)} || _ <- lists:seq(1,N)].

dist({X1,Y1},{X2,Y2}) -> math:sqrt((X1-X2)*(X1-X2) + (Y1-Y2)*(Y1-Y2)).

find_for_person(PersonLocation, SensorLocations) ->
    lists:min([{dist(PersonLocation,SL),{PersonLocation,SL}} || SL <- SensorLocations]).

find_closest(PeopleLocations, SensorsLocation) ->
    lists:min([find_for_person(PL,SensorsLocation) || PL <- PeopleLocations]).


find_closest_parallel(PeopleLocations,SensorsLocation) ->
    Parent = self(),

    _Children = [spawn(fun() -> Parent ! find_for_person(PL,SensorsLocation) end) || PL <- PeopleLocations],
    Results = [receive Result -> Result end ||  _ <- PeopleLocations ],
    lists:min(Results).

compare_speeds(Fun1, Fun2) ->
    ListPeople = sensor_dist:get_rand_locations(100),
    ListSensors = sensor_dist:get_rand_locations(10000),
    {T1,_} = timer:tc(Fun1,[ListPeople , ListSensors]),
    {T2,_} = timer:tc(Fun2,[ListPeople , ListSensors]),
    case T1 >T2 of
        true -> io:format("Fun2 is quicker ~w vs ~w~n" , [T2,T1]);
        false -> io:format("Fun1 is quicker ~w vs ~w~n", [T1,T2])
    end.

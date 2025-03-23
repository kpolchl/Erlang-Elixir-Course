%%%-------------------------------------------------------------------
%%% @author Karol Półchłopek
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Mar 2025 00:24
%%%-------------------------------------------------------------------
-module(pollution_calculator).
-author("Karol Półchłopek").

%% API
-export([report/0,number_of_readings/2,calculate_max/2,calculate_mean/2]).

report() -> [
  { {"Krakow","Jasionka"} , {{2025,5,19} , {0,29,1}}, [{"Humidity",13.3},{"PM10",10},{"Temperature",31.3},{"Preasure",11.3}] },
  { {"Krakow","Jasionka"} , {{2025,5,19} , {0,29,1}}, [{"Humidity",12.3},{"PM10",11},{"Temperature",11.3},{"Preasure",12.3}] },
  { {"Krakow","Jasionka"} , {{2025,5,19} , {0,29,1}}, [{"Humidity",14.3},{"PM10",12},{"Temperature",19.3},{"Preasure",17.3}] },
  { {"Krakow","Jasionka"} , {{2025,6,19} , {0,29,1}}, [{"Humidity",14.3},{"PM10",13},{"Temperature",18.3},{"Preasure",19.3}] },
  { {"Krakow","Jasionka"} , {{2025,1,19} , {0,29,1}}, [{"Humidity",15.3},{"PM10",14},{"Temperature",16.3},{"Preasure",19.2}] },
  { {"Krakow","Jasionka"} , {{2025,1,19} , {0,29,1}}, [{"Humidity",16.3},{"PM10",15},{"Temperature",12.3},{"Preasure",14.3}] }
].


number_of_readings([], _Date) -> 0;
number_of_readings([{_, {Date, _}, _} | T], Date) -> 1 + number_of_readings(T, Date);
number_of_readings([_|T], Date) -> number_of_readings(T, Date).


calculate_max(Readings, Type) ->
  Values = [Value || {_, _, ReadingsList} <- Readings, {T, Value} <- ReadingsList, T==Type],
  if
    length(Values) > 0 -> lists:max(Values);
    true -> io:format("No ~p in readings~n", [Type])
  end.


calculate_mean(Readings, Type) ->
  Values = [Value || {_, _, ReadingsList} <- Readings, {T, Value} <- ReadingsList, T==Type],
  if
    length(Values) > 0 -> myLists:sumFloats(Values,0)/length(Values);
    true -> io:format("No ~p in readings~n", [Type])
  end.
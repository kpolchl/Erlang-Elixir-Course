defmodule Pars do
  def parseline (linestring) do
    [datetime, type, val, id, name, coords] = linestring |> String.split(";")

    {date,time} = parse_datetime(datetime)
    %{:name => name,
      :val => val |> String.to_float(),
      :id => String.to_integer(id),
      :coords => coords
                 |> String.split(",")
                 |> Enum.map(&String.to_float()/1)
                 |> List.to_tuple(),
      :type => type,
      :date => date,
      :time => time
    }
  end

  defp identify_stations (data) do
    Enum.uniq_by(data, & &1.id)
  end

  defp parse_datetime(datetime) do
    [date_str , time_str_with_z] = String.split(datetime, "T")
    time_str = String.replace(time_str_with_z , "Z","") |> String.split(".") |> hd()
    [year,mounth , day] = date_str |> String.split("-") |> Enum.map(&String.to_integer/1)
    [hour ,minute,secound] = time_str |> String.split(":") |> Enum.map(&String.to_integer/1)
    {{year,mounth,day},{hour,minute,secound}}
  end

  def get_data() do
    raw_data = File.read!("lib/data/pollution_50k.csv/AirlyData-ALL-50k.csv")
           |> String.trim()
           |> String.split("\n")
           |>Enum.map(&Pars.parseline/1)

    identify_stations(raw_data)
  end

  # I get data from function on top paired to vairable in terminal
  def insert_readings_from_file(data) do
    Enum.each(data, fn station ->
      {hour, minute, second} = station.time
      time = Time.new!(hour, minute, second)
      {year,month,day} = station.date
      date = Date.new!(year, month, day)
      found_station = Pollutiondb.Station.find_by_name(station.name)
    Pollutiondb.Reading.add(found_station.id, date,time,station.type,station.val)
    end)
  end

  def insert_stations_from_file(data) do
    Enum.each(data, fn station ->
      {lon, lat} = station.coords
      Pollutiondb.Station.add("#{station.name}", lon, lat)
    end)
  end
end






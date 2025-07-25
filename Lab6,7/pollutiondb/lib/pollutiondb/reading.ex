defmodule Pollutiondb.Reading do
  use Ecto.Schema
  import Ecto.Query

  schema "readings" do
    field :date, :date
    field :time, :time
    field :type, :string
    field :value, :float
    belongs_to :station, Pollutiondb.Station
  end

  def add_now(station, type, value) do
    %Pollutiondb.Reading{date: Date.utc_today, time: Time.truncate(Time.utc_now(), :second), type: type , value: value , station: station}
    |> Pollutiondb.Repo.insert()
  end

  def find_by_date(date) do
    Ecto.Query.from(r in Pollutiondb.Reading,
      where: r.date ==^date, limit: 10, order_by: [desc: r.date, desc: r.time])
    |> Pollutiondb.Repo.all()
    |> Pollutiondb.Repo.preload(:station)
  end

  def find_by_id(station_id) do
    Pollutiondb.Reading
    |> where(station_id: ^station_id)
    |> order_by(desc: :date)
    |> preload(:station)  # Preload the station association
    |> Pollutiondb.Repo.all()
  end

  def add(station_id, date, time, type, value) do
    %Pollutiondb.Reading{date: date, time: time, type: type , value: value , station_id: station_id}
    |> Pollutiondb.Repo.insert()
  end
  def get_10_readings() do
    Ecto.Query.from(r in Pollutiondb.Reading,
      limit: 10, order_by: [desc: r.date, desc: r.time])
    |> Pollutiondb.Repo.all()
    |> Pollutiondb.Repo.preload(:station)
  end
end
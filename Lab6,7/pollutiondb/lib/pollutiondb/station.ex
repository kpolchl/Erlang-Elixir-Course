defmodule Pollutiondb.Station do
  use Ecto.Schema
  import Ecto.Changeset
  import Ecto.Query

  schema "stations" do
    field :name, :string
    field :lon, :float
    field :lat, :float
    has_many :readings, Pollutiondb.Reading
  end

  def get_all() do
    Pollutiondb.Repo.all(Pollutiondb.Station)
  end
  def get_by_id(id) do
    Pollutiondb.Repo.get(Pollutiondb.Station, id)
  end
  def remove(station) do
    Pollutiondb.Repo.delete(station)
  end

  def find_by_name(name) do
    case Pollutiondb.Repo.all(from s in Pollutiondb.Station, where: s.name == ^name) do
      [] -> []
      stations -> stations
    end
  end

  def find_by_location(lon,lat) do
    Ecto.Query.from(s in Pollutiondb.Station,
      where: s.lon == ^lon,
      where: s.lat == ^lat)
    |> Pollutiondb.Repo.all
  end
  def find_by_location_range(lon_min, lon_max, lat_min, lat_max) do
    Ecto.Query.from(s in Pollutiondb.Station,
      where: s.lon >= ^lon_min and s.lon <= ^lon_max,
      where: s.lat >= ^lat_min and s.lat <= ^lat_max)
    |> Pollutiondb.Repo.all
  end

  def update_name(station, newname) do
    station
    |> changeset(%{name: newname})
    |> Pollutiondb.Repo.update()
  end

  def add(name, lon, lat) do
    %Pollutiondb.Station{}
    |> changeset(%{name: name, lon: lon, lat: lat})
    |> Pollutiondb.Repo.insert()
  end

  defp changeset(station, changeset) do
    station
    |> cast(changeset, [:name, :lon, :lat])
    |> validate_required([:name, :lon, :lat])
    |> validate_number(:lat, greater_than_or_equal_to: -90, less_than_or_equal_to: 90)
    |> validate_number(:lon, greater_than_or_equal_to: -180, less_than_or_equal_to: 180)
    |> unique_constraint(:name)
  end

end
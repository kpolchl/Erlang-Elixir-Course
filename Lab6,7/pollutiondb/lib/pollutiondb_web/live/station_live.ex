defmodule PollutiondbWeb.StationLive do
  use PollutiondbWeb, :live_view
  alias Pollutiondb.Station

  def mount(_params, _session, socket) do
    socket = assign(socket,
      stations: Station.get_all(),
      name: "",
      lat: "",
      lon: "",
      query: ""
    )
    {:ok, socket}
  end

  defp to_float(val, default) do
    case Float.parse(val) do
      {float, _} -> float
      :error -> default
    end
  end

  def handle_event("insert", %{"name" => name, "lat" => lat, "lon" => lon}, socket) do
    Station.add(name, to_float(lat, 0.0), to_float(lon, 0.0))
    socket = assign(socket,
      stations: Station.get_all(),
      name: "",
      lat: "",
      lon: ""
    )
    {:noreply, socket}
  end

  def handle_event("change", %{"query" => query}, socket) do
    stations =
      if query == "" do
        Station.get_all()
      else
        case Station.find_by_name(query) do
          nil -> []  # Handle nil case
          stations -> stations
        end
      end

    socket = assign(socket, stations: stations, query: query)
    {:noreply, socket}
  end

  def render(assigns) do
    ~H"""
    <h2>Create new station</h2>
    <form phx-submit="insert">
      Name: <input type="text" name="name" value={@name} /><br/>
      Lat: <input type="number" name="lat" step="0.1" value={@lat} /><br/>
      Lon: <input type="number" name="lon" step="0.1" value={@lon} /><br/>
      <input type="submit" />
    </form>

    <h2>Search stations</h2>
    <form phx-change="change">
      Search: <input type="text" name="query" value={@query} /><br/>
    </form>

    <table>
      <tr>
        <th>Name</th>
        <th>Longitude</th>
        <th>Latitude</th>
      </tr>
      <%= for station <- @stations do %>
        <tr>
          <td><%= station.name %></td>
          <td><%= station.lon %></td>
          <td><%= station.lat %></td>
        </tr>
      <% end %>
    </table>
    """
  end
end
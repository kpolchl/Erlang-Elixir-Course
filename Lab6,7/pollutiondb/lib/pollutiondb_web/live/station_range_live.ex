defmodule PollutiondbWeb.StationRangeLive do
  use PollutiondbWeb, :live_view
  alias Pollutiondb.Station

  def mount(_params, _session, socket) do
    socket = assign(socket,
      stations: Station.get_all(),
      lat_min: 18,
      lat_max: 18,
      lon_min: 49,
      lon_max: 49
    )
    {:ok, socket}
  end

  def handle_event("update", %{"lat_min" => lat_min, "lat_max" => lat_max, "lon_min" => lon_min, "lon_max" => lon_max}, socket) do
    socket = assign(socket,
      stations: Station.find_by_location_range(String.to_integer(lon_min), String.to_integer(lon_max), String.to_integer(lat_min), String.to_integer(lat_max)),
      lat_min: String.to_integer(lat_min),
      lat_max: String.to_integer(lat_max),
      lon_min: String.to_integer(lon_min),
      lon_max: String.to_integer(lon_max)
    )
    {:noreply, socket}
  end

  def render(assigns)do
    ~H"""
    <form phx-change="update">
    Lat min <%= @lat_min %>
    <input type="range" min="18" max="21" name="lat_min" value={@lat_min}/><br/>
    Lat max <%= @lat_max %>
    <input type="range" min="18" max="21" name="lat_max" value={@lat_max}/><br/>
    Lon min <%= @lon_min %>
    <input type="range" min="49" max="51" name="lon_min" value={@lon_min}/><br/>
    Lon max <%= @lon_max %>
    <input type="range" min="49" max="51" name="lon_max" value={@lon_max}/><br/>
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

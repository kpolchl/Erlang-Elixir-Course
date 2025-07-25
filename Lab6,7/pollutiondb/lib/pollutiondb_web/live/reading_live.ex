defmodule PollutiondbWeb.ReadingLive do
  use PollutiondbWeb, :live_view
  alias Pollutiondb.Reading
  alias Pollutiondb.Station

  def mount(_params, _session, socket) do
    socket = assign(socket,
      stations: Station.get_all(),
      readings: Reading.get_10_readings(),
      date: Date.utc_today,
      station_id: 0,
      time: Time.utc_now,
      type: "",
      val: 0
    )
    {:ok, socket}
  end

  def handle_event("update", %{"date" => date}, socket) do
    socket = assign(socket,
      readings: Reading.find_by_date(date),
      date: date
    )
    {:noreply, socket}
  end

  def handle_event("insert", params, socket) do
    %{
      "date" => date_str,
      "time" => time_str,
      "type" => type,
      "val" => val_str,
      "station_id" => station_id_str
    } = params

    with {val_num, ""} <- Float.parse(val_str),
         {station_id_num, ""} <- Integer.parse(station_id_str),
         {:ok, date} <- Date.from_iso8601(date_str),
         {:ok, time} <- parse_time(time_str),
         {:ok, _} <- Reading.add(station_id_num, date, time, type, val_num) do
      {:noreply, update_socket(socket)}
    else
      error -> handle_error(error, socket)
    end
  end

  defp parse_time(time_str) do
    case String.split(time_str, ":") do
      [h, m] -> Time.new(String.to_integer(h), String.to_integer(m), 0)
      [h, m, s] -> Time.new(String.to_integer(h), String.to_integer(m), String.to_integer(s))
      _ -> {:error, :invalid_time}
    end
  end

  defp update_socket(socket) do
    assign(socket,
      date: Date.utc_today(),
      time: Time.utc_now(),
      type: "",
      val: 0.0
    )
  end

  defp handle_error(:error, socket), do: put_flash(socket, :error, "Invalid number")
  defp handle_error({:error, _}, socket), do: put_flash(socket, :error, "Invalid date/time")


  def render(assigns) do
    ~H"""
    <form phx-submit="insert">
      <select name="station_id">
        <%= for station <- @stations do %>
        <option label={station.name} value={station.id} selected={station.id == @station_id}/>
        <% end %>
      </select>
      Date: <input type="date" name="date" value={@date} /><br/>
      Time: <input type="time" name="time" step="1" value={@time} /><br/>
      Type: <input type="text" name="type" value={@type} /><br/>
      Value: <input type="number" name="val" step="0.1" value={@val} /><br/>
      <input type="submit" />
    </form>




    <form phx-change="update">
    Date
    <input type="date" name="date" value={@date}/><br/>
    </form>
    <table>
      <tr>
        <th>Name</th>
        <th>Date</th>
        <th>Time</th>
        <th>Type</th>
        <th>Value</th>
      </tr>
      <%= for reading <- @readings do %>
        <tr>
          <td><%= reading.station.name %></td>
          <td><%= reading.date %></td>
          <td><%= reading.time %></td>
          <td><%= reading.type %></td>
          <td><%= reading.value %></td>
        </tr>
      <% end %>
    </table>

    """
  end

end

defmodule MusicJamServerWeb.RoomChannel do
  use Phoenix.Channel

  def join("room:lobby", _message, socket) do
    {:ok, socket}
  end

  def join("room:" <> _private_room_id, _params, _socket) do
    {:error, %{reason: "unauthorized"}}
  end

  def handle_in("playSound", payload, socket) do
    broadcast_from!(socket, "playSound", payload)
    {:noreply, socket}
  end

end

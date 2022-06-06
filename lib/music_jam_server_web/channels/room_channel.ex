defmodule MusicJamServerWeb.RoomChannel do
  use Phoenix.Channel

  def join("room:lobby", _message, socket) do
    {:ok, socket}
  end

  def join("room:" <> _private_room_id, _params, _socket) do
    {:error, %{reason: "unauthorized"}}
  end

  def handle_in("update_instrument", payload, socket) do
    broadcast_from!(socket, "update_instrument", payload)
    {:noreply, socket}
  end

end

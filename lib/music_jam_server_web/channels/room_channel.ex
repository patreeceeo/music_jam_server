defmodule MusicJamServerWeb.RoomChannel do
  use Phoenix.Channel

  def join("room:lobby", _message, socket) do
    {:ok, socket}
  end

  def join("room:" <> _private_room_id, _params, _socket) do
    {:error, %{reason: "unauthorized"}}
  end

  def handle_in("update_instrument", %{"body" => body}, socket) do
    broadcast!(socket, "update_instrument", %{"body" => body})
    {:noreply, socket}
  end

end

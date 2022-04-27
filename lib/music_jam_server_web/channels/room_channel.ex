defmodule MusicJamServerWeb.RoomChannel do
  use Phoenix.Channel

  def join("room:lobby", _message, socket) do
    {:ok, socket}
  end

  def join("room:" <> _private_room_id, _params, _socket) do
    {:error, %{reason: "unauthorized"}}
  end

  def handle_in("button_down", %{}, socket) do
    broadcast!(socket, "button_down", %{})
    {:noreply, socket}
  end

  def handle_in("button_up", %{}, socket) do
    broadcast!(socket, "button_up", %{})
    {:noreply, socket}
  end
end

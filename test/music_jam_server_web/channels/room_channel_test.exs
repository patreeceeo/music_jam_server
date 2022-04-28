defmodule MusicJamServerWeb.RoomChannelTest do
  use MusicJamServerWeb.ChannelCase

  setup do
    {:ok, _, socket} =
      MusicJamServerWeb.UserSocket
      |> socket("user_id", %{some: :assign})
      |> subscribe_and_join(MusicJamServerWeb.RoomChannel, "room:lobby")

    %{socket: socket}
  end

  test "broadcasts instrument updates to room:{id}", %{socket: socket} do
    push(socket, "update_instrument", %{"body" => "muzak"})
    assert_broadcast "update_instrument", %{"body" => "muzak"}
  end
end

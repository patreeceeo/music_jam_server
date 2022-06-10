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
    msgBody = %{"pitch" => 32, "soundId" => "didgeridoo", "voiceIndex" => 0, "volume" => 1.2}
    push(socket, "playSound", msgBody)
    assert_broadcast "playSound", msgBbody
  end
end

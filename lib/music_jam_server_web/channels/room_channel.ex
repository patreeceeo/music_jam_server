defmodule MusicJamServerWeb.RoomChannel do
  use Phoenix.Channel
  require Logger

  def join("room:lobby", _message, socket) do
    {:ok, socket}
  end

  def join("room:" <> _private_room_id, _params, _socket) do
    {:error, %{reason: "unauthorized"}}
  end

  def handle_in("playNote", payload, socket) do
    %{"pitch" => pitch, "soundId" => _, "voiceIndex" => voiceIndex, "volume" => volume} = payload
    Logger.info("playNote {pitch: #{pitch}, voiceIndex: #{voiceIndex}, volume: #{volume}}")
    broadcast_from!(socket, "playNote", payload)
    {:noreply, socket}
  end
end

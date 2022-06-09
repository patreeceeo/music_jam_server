defmodule MusicJamServerWeb.RoomChannel do
  use Phoenix.Channel
  require Logger

  def join("room:lobby", _message, socket) do
    {:ok, socket}
  end

  def join("room:" <> _private_room_id, _params, _socket) do
    {:error, %{reason: "unauthorized"}}
  end

  def handle_in("playSound", payload, socket) do
    %{"pitch" => pitch, "soundId" => _, "voiceIndex" => voiceIndex, "volume" => volume} = payload
    Logger.info("playSound {pitch: #{pitch}, voiceIndex: #{voiceIndex}, volume: #{volume}}")
    broadcast_from!(socket, "playSound", payload)
    {:noreply, socket}
  end

end

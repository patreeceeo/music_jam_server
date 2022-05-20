defmodule MusicJamServerWeb.PageLive do
  use MusicJamServerWeb, :live_view

  def mount(_params, _assigns, socket) do
    Registry.register(Registry.PubSub, "room:lobby", nil)
    {:ok, assign(socket, playing_voice_ids: MapSet.new(), timer_value: 0)}
  end

  def render(assigns) do
    Phoenix.View.render(MusicJamServerWeb.PageView, "fretboard.html", %{
      fretboard: MusicJamServer.Insts.Fretboard,
      playing_voice_ids: assigns[:playing_voice_ids] }
    )
  end

  # handle messages from channel
  def handle_info({"update_instrument", %{ "voice_id" => voice_id, "volume" => volume, "pitch" => _pitch, "sliding" => _sliding }}, socket) when volume > 0 do
    map = socket.assigns[:playing_voice_ids]
    {:noreply, assign(socket, playing_voice_ids: MapSet.put(map, voice_id))}
  end

  def handle_info({"update_instrument", %{ "voice_id" => voice_id, "volume" => volume, "pitch" => _pitch, "sliding" => _sliding }}, socket) when volume === 0 do
    map = socket.assigns[:playing_voice_ids]
    {:noreply, assign(socket, playing_voice_ids: MapSet.delete(map, voice_id))}
  end
end

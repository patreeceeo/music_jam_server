defmodule MusicJamServerWeb.PageLive do
  use MusicJamServerWeb, :live_view

  def mount(_params, _assigns, socket) do
    Registry.register(Registry.PubSub, "room:lobby", nil)
    {:ok, assign(socket, playing_voice_ids: MapSet.new())}
  end

  def render(assigns) do
    Phoenix.View.render(MusicJamServerWeb.PageView, "fretboard.html", %{
      fretboard: MusicJamServer.Insts.Fretboard,
      playing_voice_ids: assigns[:playing_voice_ids] }
    )
  end
end

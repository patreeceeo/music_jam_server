defmodule MusicJamServerWeb.PageLive do
  use MusicJamServerWeb, :live_view

  def mount(_params, _assigns, socket) do
    {:ok, assign(socket, playing_string_index: -1)}
  end

  def render(assigns) do
    Phoenix.View.render(MusicJamServerWeb.PageView, "fretboard.html", %{
      fretboard: MusicJamServer.Insts.Fretboard,
      playing_string_index: assigns[:playing_string_index] }
    )
  end

  def handle_event("play_voice", %{ "string_index" => string_index }, socket) do
    IO.inspect(string_index, label: :playing_string_index)
    {:noreply, assign(socket, playing_string_index: string_index)}
  end
end

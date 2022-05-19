defmodule MusicJamServerWeb.PageView do
  use MusicJamServerWeb, :view
  alias Phoenix.LiveView.JS

  def to_scalar(note) do
    MusicJamServer.MusicNotes.to_scalar(note)
  end

end

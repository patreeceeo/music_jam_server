defmodule MusicJamServerWeb.PageView do
  use MusicJamServerWeb, :view

  def instrument() do
    Jason.encode!(
      %{
        voices: MusicJamServer.Insts.Fretboard.json()
      },
      escape: :html_safe,
      map: :strict,
      pretty: true
    )
  end
end

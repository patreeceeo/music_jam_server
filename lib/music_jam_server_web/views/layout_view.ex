defmodule MusicJamServerWeb.LayoutView do
  use MusicJamServerWeb, :view

  # Phoenix LiveDashboard is available only in development by default,
  # so we instruct Elixir to not warn if the dashboard route is missing.
  @compile {:no_warn_undefined, {Routes, :live_dashboard_path, 2}}

  # def render("fretboard.json", _assigns) do
  # end

  def instrument() do
    Jason.encode!(%{
      voices: MusicJamServer.Insts.Fretboard.json(),
    },
      escape: :html_safe, map: :strict, pretty: true
    )
  end
end

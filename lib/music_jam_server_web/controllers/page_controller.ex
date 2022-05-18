defmodule MusicJamServerWeb.PageController do
  use MusicJamServerWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end


  def show_fretboard(conn, _params) do
    render(conn, "fretboard.html", %{ fretboard: MusicJamServer.Insts.Fretboard })
  end
end

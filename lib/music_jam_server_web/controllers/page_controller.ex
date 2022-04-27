defmodule MusicJamServerWeb.PageController do
  use MusicJamServerWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end

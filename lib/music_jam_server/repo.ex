defmodule MusicJamServer.Repo do
  use Ecto.Repo,
    otp_app: :music_jam_server,
    adapter: Ecto.Adapters.Postgres
end

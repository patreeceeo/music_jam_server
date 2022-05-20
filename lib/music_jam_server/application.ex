defmodule MusicJamServer.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Start the Ecto repository
      MusicJamServer.Repo,
      # Start the Telemetry supervisor
      MusicJamServerWeb.Telemetry,
      # Start the PubSub system
      {Phoenix.PubSub, name: MusicJamServer.PubSub},
      # Start the Endpoint (http/https)
      MusicJamServerWeb.Endpoint,
      # Start a worker by calling: MusicJamServer.Worker.start_link(arg)
      # {MusicJamServer.Worker, arg}
      {Registry, [keys: :duplicate, name: Registry.PubSub]}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: MusicJamServer.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    MusicJamServerWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end

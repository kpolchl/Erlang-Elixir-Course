# This file is responsible for configuring your application
# and its dependencies with the aid of the Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
import Config

config :pollutiondb,
  generators: [timestamp_type: :utc_datetime]

# Configures the endpoint
config :pollutiondb, PollutiondbWeb.Endpoint,
  url: [host: "localhost"],
  adapter: Bandit.PhoenixAdapter,
  render_errors: [
    formats: [html: PollutiondbWeb.ErrorHTML, json: PollutiondbWeb.ErrorJSON],
    layout: false
  ],
  pubsub_server: Pollutiondb.PubSub,
  live_view: [signing_salt: "hGvGgreZ"]

# Configures the mailer
#
# By default it uses the "Local" adapter which stores the emails
# locally. You can see the emails in your browser, at "/dev/mailbox".
#
# For production it's recommended to configure a different adapter
# at the `config/runtime.exs`.
config :pollutiondb, Pollutiondb.Mailer, adapter: Swoosh.Adapters.Local

# Configure esbuild (the version is required)
config :esbuild,
  version: "0.17.11",
  pollutiondb: [
    args:
      ~w(js/app.js --bundle --target=es2017 --outdir=../priv/static/assets --external:/fonts/* --external:/images/*),
    cd: Path.expand("../assets", __DIR__),
    env: %{"NODE_PATH" => Path.expand("../deps", __DIR__)}
  ]

# Configure tailwind (the version is required)
config :tailwind,
  version: "3.4.0",
  pollutiondb: [
    args: ~w(
      --config=tailwind.config.js
      --input=css/app.css
      --output=../priv/static/assets/app.css
    ),
    cd: Path.expand("../assets", __DIR__)
  ]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{config_env()}.exs"

config :pollutiondb, Pollutiondb.Repo,
  database: "pollutiondb_repo",
  username: "user",
  password: "pass",
  hostname: "localhost"

config :pollutiondb, ecto_repos: [Pollutiondb.Repo]
config :pollutiondb, Pollutiondb.Repo, database: "database/pollutiondb.db"

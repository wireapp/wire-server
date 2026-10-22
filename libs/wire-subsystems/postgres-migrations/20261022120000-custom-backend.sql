CREATE TABLE IF NOT EXISTS custom_backend (
  domain text PRIMARY KEY,
  config_json_url bytea NOT NULL,
  webapp_welcome_url bytea NOT NULL
);

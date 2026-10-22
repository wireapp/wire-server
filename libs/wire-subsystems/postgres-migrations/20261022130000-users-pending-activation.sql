-- WPB-29576: migrate user pending activation store from Cassandra to Postgres
CREATE TABLE IF NOT EXISTS users_pending_activation (
  user_id uuid PRIMARY KEY,
  expires_at timestamptz NOT NULL
);

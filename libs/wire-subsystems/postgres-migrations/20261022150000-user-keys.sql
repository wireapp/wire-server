-- WPB-29575: migrate user key store from Cassandra to Postgres
CREATE TABLE IF NOT EXISTS user_keys (
  key text PRIMARY KEY,
  user_id uuid NOT NULL
);

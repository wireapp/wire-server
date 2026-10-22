CREATE TABLE IF NOT EXISTS properties (
  user_id uuid NOT NULL,
  key text NOT NULL,
  value bytea NOT NULL,
  PRIMARY KEY (user_id, key)
);

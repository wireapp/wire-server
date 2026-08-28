-- WPB-28377: gundeck presence (replaces redis presence hashes)
CREATE TABLE IF NOT EXISTS presence (
  user_id uuid NOT NULL,
  conn_id text NOT NULL,
  resource text NOT NULL,
  client_id text,
  created_at bigint NOT NULL,
  PRIMARY KEY (user_id, conn_id)
);

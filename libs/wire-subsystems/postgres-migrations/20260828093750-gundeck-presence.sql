-- WPB-28377: gundeck presence (replaces redis presence hashes)
CREATE TABLE IF NOT EXISTS presence (
  user_id uuid NOT NULL,
  conn_id text NOT NULL,
  resource text NOT NULL,
  client_id text,
  created_at timestamptz NOT NULL,
  PRIMARY KEY (user_id, conn_id)
);

-- index for cleanup deletes like `DELETE ... WHERE created_at < now() - interval '7 days'`
CREATE INDEX presence_created_at_idx ON presence (created_at);

-- WPB-28377: gundeck presence (replaces redis presence hashes)
CREATE TABLE IF NOT EXISTS presence (
  user_id uuid NOT NULL,
  conn_id text NOT NULL,
  resource text NOT NULL,
  client_id text,
  created_at timestamptz NOT NULL,
  -- absolute TTL deadline (created_at + 7 days, mirroring the redis key TTL);
  -- filtered on read and garbage-collected by pg_cron (ADR-0005)
  expires_at timestamptz NOT NULL,
  PRIMARY KEY (user_id, conn_id)
);

-- index for the pg_cron TTL deletes like `DELETE ... WHERE expires_at < now()`
CREATE INDEX presence_expires_at_idx ON presence (expires_at);

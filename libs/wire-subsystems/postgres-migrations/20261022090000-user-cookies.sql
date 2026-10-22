CREATE TABLE IF NOT EXISTS user_cookies (
  user_id uuid NOT NULL,
  id int8 NOT NULL,
  typ int4 NOT NULL,
  created timestamptz NOT NULL,
  label text,
  succ_id int8,
  expires_at timestamptz NOT NULL,
  -- Replaces the Cassandra row TTL: the row is only valid for lookups while
  -- now() < invalid_after. Superseded cookies are re-inserted with a short
  -- TTL (renew age), which lands here.
  invalid_after timestamptz NOT NULL,
  PRIMARY KEY (user_id, id)
);

CREATE INDEX IF NOT EXISTS user_cookies_expires_idx ON user_cookies (user_id, expires_at);

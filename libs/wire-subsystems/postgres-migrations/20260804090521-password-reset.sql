CREATE TABLE IF NOT EXISTS password_reset (
  key text PRIMARY KEY,
  code text NOT NULL,
  "user" uuid NOT NULL,
  retries int4,
  timeout timestamptz,
  expires_at timestamptz NOT NULL
);

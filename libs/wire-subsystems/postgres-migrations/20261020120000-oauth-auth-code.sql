-- OAuth PKCE authorization codes (mirrors Cassandra table oauth_auth_code, V74+V75).
-- Codes are single-use and expire after the configured TTL; consumers must
-- filter on expires_at (see Wire.OAuthAuthCodeStore.Postgres).
CREATE TABLE IF NOT EXISTS oauth_auth_code (
  code TEXT PRIMARY KEY,
  client UUID NOT NULL,
  "user" UUID NOT NULL,
  scope TEXT[] NOT NULL,
  redirect_uri BYTEA NOT NULL,
  code_challenge TEXT,
  expires_at TIMESTAMPTZ NOT NULL
);
CREATE INDEX IF NOT EXISTS oauth_auth_code_expires_at_idx ON oauth_auth_code (expires_at);

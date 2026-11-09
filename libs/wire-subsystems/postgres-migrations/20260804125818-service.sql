CREATE TABLE IF NOT EXISTS service (
  provider uuid NOT NULL,
  id uuid NOT NULL,
  base_url bytea NOT NULL,
  auth_token text NOT NULL,
  fingerprints bytea[] NOT NULL DEFAULT '{}',
  enabled boolean NOT NULL,
  last_update timestamptz NOT NULL,
  PRIMARY KEY (provider, id)
);

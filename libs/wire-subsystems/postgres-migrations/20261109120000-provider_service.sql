-- WPB-22961: brig provider-platform service tables

CREATE TABLE IF NOT EXISTS provider_service (
  provider uuid NOT NULL,
  id uuid NOT NULL,
  base_url bytea NOT NULL,
  name text NOT NULL,
  summary text,
  descr text NOT NULL,
  auth_tokens text[] NOT NULL,
  pubkeys jsonb NOT NULL,
  fingerprints bytea[] NOT NULL,
  assets jsonb NOT NULL,
  tags bigint[] NOT NULL,
  enabled boolean NOT NULL,
  last_update timestamptz NOT NULL,
  PRIMARY KEY (provider, id)
);

CREATE TABLE IF NOT EXISTS provider_service_tag (
  bucket int NOT NULL,
  tag bigint NOT NULL,
  name text NOT NULL,
  service uuid NOT NULL,
  provider uuid NOT NULL,
  PRIMARY KEY (bucket, tag, name, service)
);

CREATE TABLE IF NOT EXISTS provider_service_prefix (
  prefix text NOT NULL,
  name text NOT NULL,
  service uuid NOT NULL,
  provider uuid NOT NULL,
  PRIMARY KEY (prefix, name, service)
);

CREATE TABLE IF NOT EXISTS provider_service_whitelist (
  team uuid NOT NULL,
  provider uuid NOT NULL,
  service uuid NOT NULL,
  PRIMARY KEY (team, provider, service)
);

CREATE TABLE IF NOT EXISTS provider_service_whitelist_rev (
  provider uuid NOT NULL,
  service uuid NOT NULL,
  team uuid NOT NULL,
  PRIMARY KEY (provider, service, team)
);

CREATE TABLE IF NOT EXISTS scim_token (
  token text PRIMARY KEY,
  team uuid NOT NULL,
  id uuid NOT NULL,
  created_at timestamptz NOT NULL,
  idp uuid,
  descr text NOT NULL,
  name text
);
CREATE INDEX IF NOT EXISTS scim_token_by_team_id ON scim_token (team, id);

CREATE TABLE IF NOT EXISTS scim_external (
  team uuid NOT NULL,
  external_id text NOT NULL,
  "user" uuid NOT NULL,
  creation_status int4,
  PRIMARY KEY (team, external_id)
);

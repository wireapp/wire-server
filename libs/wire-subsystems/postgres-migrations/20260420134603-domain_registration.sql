CREATE TABLE domain_registration (
  domain text PRIMARY KEY,
  authorized_team uuid,
  domain_redirect integer,
  team_invite integer,
  idp_id uuid,
  backend_url bytea,
  team uuid,
  dns_verification_token text,
  ownership_token_hash bytea,
  webapp_url bytea
);

CREATE INDEX domain_registration_authorized_team_idx
  ON domain_registration (authorized_team);

CREATE TABLE domain_registration_challenge (
  id uuid PRIMARY KEY,
  domain text NOT NULL,
  challenge_token_hash bytea NOT NULL,
  dns_verification_token text NOT NULL,
  expires_at timestamptz NOT NULL
);

CREATE INDEX domain_registration_challenge_expires_at_idx
  ON domain_registration_challenge (expires_at);


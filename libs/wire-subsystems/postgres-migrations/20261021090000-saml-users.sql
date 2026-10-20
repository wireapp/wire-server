CREATE TABLE IF NOT EXISTS spar_user (
  issuer text NOT NULL,
  normalized_uname_id text NOT NULL,
  sso_id text NOT NULL,
  uid uuid NOT NULL,
  PRIMARY KEY (issuer, normalized_uname_id)
);

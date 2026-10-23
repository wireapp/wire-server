-- Client store (brig): clients, prekeys and MLS public keys.
CREATE TABLE IF NOT EXISTS clients (
  user_id uuid NOT NULL,
  client_id text NOT NULL,
  type int NOT NULL,
  tstamp timestamptz NOT NULL,
  label text,
  class int,
  cookie text,
  model text,
  capabilities int[],
  last_active timestamptz,
  PRIMARY KEY (user_id, client_id)
);

CREATE TABLE IF NOT EXISTS prekeys (
  user_id uuid NOT NULL,
  client_id text NOT NULL,
  key int NOT NULL,
  data text NOT NULL,
  PRIMARY KEY (user_id, client_id, key)
);

CREATE TABLE IF NOT EXISTS mls_public_keys (
  user_id uuid NOT NULL,
  client_id text NOT NULL,
  sig_scheme text NOT NULL,
  key bytea NOT NULL,
  PRIMARY KEY (user_id, client_id, sig_scheme)
);

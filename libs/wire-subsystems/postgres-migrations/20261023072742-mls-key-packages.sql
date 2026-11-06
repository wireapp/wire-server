CREATE TABLE IF NOT EXISTS mls_key_packages (
  user_id uuid NOT NULL,
  client text NOT NULL,
  cipher_suite int NOT NULL,
  ref bytea NOT NULL,
  data bytea NOT NULL,
  PRIMARY KEY (user_id, client, cipher_suite, ref)
);

CREATE TABLE IF NOT EXISTS mls_key_packages_migrated_users (
  user_id uuid NOT NULL,
  PRIMARY KEY (user_id)
);

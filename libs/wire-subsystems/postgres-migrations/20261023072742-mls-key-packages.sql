CREATE TABLE IF NOT EXISTS mls_key_packages (
  "user" uuid NOT NULL,
  client text NOT NULL,
  cipher_suite int NOT NULL,
  ref bytea NOT NULL,
  data bytea NOT NULL,
  PRIMARY KEY ("user", client, cipher_suite, ref)
);

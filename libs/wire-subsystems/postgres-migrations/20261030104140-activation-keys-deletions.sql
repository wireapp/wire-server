-- WPB-22970: copy-loop deletion markers for the activation-keys Cassandra ->
-- PostgreSQL migration. Dual-write deletes record the deleted key here so the
-- copy loop's INSERT ... WHERE NOT EXISTS cannot resurrect a row that was
-- deleted (e.g. consumed) after a copy pass. See Wire.ActivationCodeStore.Migration.
CREATE TABLE activation_keys_deletions (
  key text NOT NULL,
  deleted_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (key)
);

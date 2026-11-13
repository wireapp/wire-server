-- WPB-22970: copy-loop deletion markers for the activation-keys Cassandra ->
-- PostgreSQL migration. Dual-write deletes record the deleted key here so the
-- copy loop's INSERT ... WHERE NOT EXISTS cannot resurrect a row that was
-- deleted (e.g. consumed) after a copy pass. See Wire.ActivationCodeStore.Migration.
--
-- Teardown: once the cutover to PostgreSQL-only is complete (Cassandra
-- retired), drop this table — it serves no purpose afterwards and retains
-- email-derived keys, so it must not outlive the migration.
CREATE TABLE activation_keys_deletions (
  key text NOT NULL,
  deleted_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (key)
);

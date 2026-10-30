-- WPB-29580: copy-loop deletion markers for the provider-password Cassandra ->
-- PostgreSQL migration. Dual-write deletes record the deleted provider id here
-- so the copy loop's INSERT ... WHERE NOT EXISTS cannot resurrect a credential
-- that was deleted after a copy pass. See Wire.PasswordStore.Migration.
--
-- Teardown: this table exists only for the duration of the migration. Once the
-- cutover to PostgreSQL-only is complete (Cassandra retired), drop it — it
-- serves no purpose afterwards and must not accumulate provider ids.
CREATE TABLE provider_passwords_deletions (
  id uuid NOT NULL,
  deleted_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (id)
);

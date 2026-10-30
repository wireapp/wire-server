-- WPB-29577: copy-loop deletion markers for the verification-codes Cassandra ->
-- PostgreSQL migration. Dual-write deletes record the deleted (key, scope) here
-- so the copy loop's INSERT ... WHERE NOT EXISTS cannot resurrect a row that
-- was deleted after a copy pass. See Wire.VerificationCodeStore.Migration.
CREATE TABLE vcodes_deletions (
  key text NOT NULL,
  scope int4 NOT NULL,
  deleted_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (key, scope)
);

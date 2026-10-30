-- WPB-29576: copy-loop deletion markers for the users-pending-activation
-- Cassandra -> PostgreSQL migration. Dual-write deletes record the deleted
-- user ids here so the copy loop's INSERT ... WHERE NOT EXISTS cannot resurrect
-- a row that was removed after a copy pass. See
-- Wire.UserPendingActivationStore.Migration.
CREATE TABLE users_pending_activation_deletions (
  user_id uuid NOT NULL,
  deleted_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (user_id)
);

-- Backup API: incremental files of an epoch's current full

CREATE TABLE IF NOT EXISTS backup_incrementals (
  id         uuid        PRIMARY KEY,
  full_id    uuid        NOT NULL
             REFERENCES backup_epochs (full_id)
             ON UPDATE RESTRICT ON DELETE RESTRICT,
  size       bigint      NOT NULL CHECK (size >= 0),
  created_at timestamptz NOT NULL
);

CREATE INDEX IF NOT EXISTS backup_incrementals_full_id ON backup_incrementals (full_id);

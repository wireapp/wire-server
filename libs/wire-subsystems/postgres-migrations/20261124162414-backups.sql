-- Backup API: files of client backups (S3 objects `backups/<id>`) and their epochs

CREATE TABLE IF NOT EXISTS backup_epochs (
  user_id         uuid        NOT NULL,
  client_id       text        NOT NULL,
  epoch_no        bigint      NOT NULL CHECK (epoch_no BETWEEN 1 AND 4294967295),
  created_at      timestamptz NOT NULL,
  full_id         uuid        NOT NULL UNIQUE,
  full_size       bigint      NOT NULL CHECK (full_size >= 0),
  full_created_at timestamptz NOT NULL,
  PRIMARY KEY (user_id, client_id, epoch_no)
);

-- Upload reservations and deferred deletions of S3 objects
CREATE TABLE IF NOT EXISTS backup_orphans (
  id                  uuid        PRIMARY KEY,
  user_id             uuid        NOT NULL,
  client_id           text        NOT NULL,
  epoch_no            bigint      NOT NULL,
  file_created_at     timestamptz,
  orphaned_at         timestamptz NOT NULL DEFAULT now(),
  deletion_last_tried timestamptz,
  -- never written by the app
  kind                text GENERATED ALWAYS AS
    (CASE WHEN file_created_at IS NULL THEN 'upload' ELSE 'deletion' END) STORED
);

CREATE INDEX IF NOT EXISTS backup_orphans_orphaned_at ON backup_orphans (orphaned_at);

-- Upload reservations of a client (blockReservations, listBackupClients)
CREATE INDEX IF NOT EXISTS backup_orphans_reservations
  ON backup_orphans (user_id, client_id) WHERE file_created_at IS NULL;

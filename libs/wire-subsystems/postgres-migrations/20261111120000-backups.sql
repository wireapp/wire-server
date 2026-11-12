-- WPB-25648: automatic backups metadata

CREATE TABLE IF NOT EXISTS backup_files (
  id uuid PRIMARY KEY,
  user_id uuid NOT NULL,
  client_id text NOT NULL,
  epoch bigint NOT NULL CHECK (epoch BETWEEN 1 AND 4294967295),
  kind smallint NOT NULL CHECK (kind IN (0, 1)), -- 0 full, 1 incremental
  size bigint NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now()
);
CREATE INDEX IF NOT EXISTS backup_files_user_client_epoch
  ON backup_files (user_id, client_id, epoch, created_at);

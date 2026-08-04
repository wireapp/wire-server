CREATE TABLE mls_commit_locks (
  group_id bytea NOT NULL,
  epoch bigint NOT NULL,
  expires_at timestamptz NOT NULL,
  PRIMARY KEY (group_id, epoch)
);

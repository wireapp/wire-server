-- PostgreSQL storage for team notifications (WPB-22958).
-- No data is copied from Cassandra: switching galley's
-- postgresMigration.teamNotifications to postgresql starts with an empty table.
-- payload is the JSON-encoded notification payload (mirrors the Cassandra
-- blob column as bytea).
-- occurred_at is the RFC 4122 v1 timestamp of the notification id;
-- expires_at is occurred_at + 28 days (the Cassandra TTL).

CREATE TABLE team_notifications (
  team uuid NOT NULL,
  id uuid NOT NULL,
  payload bytea NOT NULL,
  occurred_at timestamptz NOT NULL,
  expires_at timestamptz NOT NULL,
  PRIMARY KEY (team, id)
);

-- read path: WHERE team = ? AND expires_at > now() ORDER BY occurred_at, id
CREATE INDEX team_notifications_team_occurred_at_idx
  ON team_notifications (team, occurred_at, id);

-- TTL garbage collection per ADR-0005: DELETE FROM team_notifications WHERE expires_at < now()
CREATE INDEX team_notifications_expires_at_idx
  ON team_notifications (expires_at);

-- TTL garbage collection per ADR-0005. Hourly: 28d TTL and low write volume
-- (member-join events only) — no need for the 5-minute cadence of short-lived
-- data, and volume is far below the batching threshold of ADR-0005.
-- The purge job is scheduled by 20261110130000-team-notifications-ttl-cleanup.sql.
-- pg_cron itself (superuser + shared_preload_libraries) remains an
-- operator/DBA prerequisite, NOT part of this migration — no migration on
-- develop creates extensions. Without pg_cron the job is skipped (the
-- migration only raises a warning) and expired rows are filtered out by
-- every read.

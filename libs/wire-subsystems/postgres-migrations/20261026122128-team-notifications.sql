-- Migration of team notifications from Cassandra to PostgreSQL (WPB-22958).
-- payload is a JSON blob (Cassandra blob column type mirrored as bytea);
-- the copy loop re-serializes the decoded payload through the shared
-- insert path, so bytes are aeson-normalized, semantically equivalent.
-- occurred_at is the RFC 4122 v1 timestamp of the notification id;
-- expires_at is anchored at the occurrence so rows copied late keep their
-- original 28-day TTL.

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
  ON team_notifications (team, occurred_at);

-- TTL garbage collection per ADR-0005: DELETE FROM team_notifications WHERE expires_at < now()
CREATE INDEX team_notifications_expires_at_idx
  ON team_notifications (expires_at);

-- TTL garbage collection per ADR-0005. Hourly: 28d TTL and low write volume
-- (member-join events only) — no need for the 5-minute cadence of short-lived
-- data, and volume is far below the batching threshold of ADR-0005.
-- Requires pg_cron: CREATE EXTENSION pg_cron (superuser +
-- shared_preload_libraries) is an operator/DBA prerequisite, NOT part of this
-- migration — no migration on develop creates extensions; they are
-- provisioned externally.
SELECT cron.schedule(
  'expire-team-notifications',
  '0 * * * *',
  $$DELETE FROM team_notifications WHERE expires_at < now()$$
);

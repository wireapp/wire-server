-- Migration of user notifications from Cassandra to PostgreSQL (WPB-22971).
-- Mirrors the Cassandra `notifications` / `notification_payload` tables
-- (gundeck keyspace). `user` is a reserved word in PostgreSQL, so the column
-- is renamed `user_id`. `clients` (Cassandra set<text>) becomes text[].
-- Payload bytes are copied raw by the copy loop, so the bytea columns hold
-- exactly the Cassandra bytes. occurred_at is the RFC 4122 v1 timestamp of
-- the notification id (payload ids are v4 and anchor at the copy time);
-- expires_at plays the role of the Cassandra row TTL.

CREATE TABLE notifications (
  user_id uuid NOT NULL,
  id uuid NOT NULL,
  payload bytea,
  payload_ref uuid,
  payload_ref_size int4,
  clients text[] NOT NULL DEFAULT '{}',
  occurred_at timestamptz NOT NULL,
  expires_at timestamptz NOT NULL,
  PRIMARY KEY (user_id, id)
);

-- read path: WHERE user_id = ? AND expires_at > now() ORDER BY occurred_at, id
CREATE INDEX notifications_user_occurred_at_idx
  ON notifications (user_id, occurred_at);

-- TTL garbage collection per ADR-0005: DELETE FROM notifications WHERE expires_at < now()
CREATE INDEX notifications_expires_at_idx
  ON notifications (expires_at);

-- resolution of payload_ref fan-out rows
CREATE INDEX notifications_payload_ref_idx
  ON notifications (payload_ref);

CREATE TABLE notification_payload (
  id uuid PRIMARY KEY,
  payload bytea NOT NULL,
  occurred_at timestamptz NOT NULL,
  expires_at timestamptz NOT NULL
);

-- TTL garbage collection per ADR-0005
CREATE INDEX notification_payload_expires_at_idx
  ON notification_payload (expires_at);

-- TTL garbage collection per ADR-0005. Hourly: notification TTLs are 28 days
-- and, like team_notifications, the volume is far below the batching
-- threshold of ADR-0005. Requires pg_cron: CREATE EXTENSION pg_cron
-- (superuser + shared_preload_libraries) is an operator/DBA prerequisite,
-- NOT part of this migration — no migration on develop creates extensions;
-- they are provisioned externally.
SELECT cron.schedule(
  'expire-notifications',
  '0 * * * *',
  $$DELETE FROM notifications WHERE expires_at < now()$$
);

SELECT cron.schedule(
  'expire-notification-payload',
  '0 * * * *',
  $$DELETE FROM notification_payload WHERE expires_at < now()$$
);

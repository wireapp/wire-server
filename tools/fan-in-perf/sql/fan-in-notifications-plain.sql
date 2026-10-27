-- Used by fan-in-perf only (not a migration). SchemaShape = Plain (baseline).
-- Runs after fan-in-common.sql.

-- Event Storage
CREATE TABLE user_notifications (
  user_id UUID NOT NULL,
  notification_id UUID NOT NULL,
  payload jsonb NOT NULL,
  origin UUID,
  PRIMARY KEY (user_id, notification_id)
);

CREATE TABLE client_notifications (
  user_id UUID NOT NULL,
  client_id bigint NOT NULL,
  notification_id UUID NOT NULL,
  payload jsonb NOT NULL,
  origin UUID,
  PRIMARY KEY (user_id, client_id, notification_id)
);

CREATE TABLE team_notifications (
  team_id UUID NOT NULL,
  notification_id UUID NOT NULL,
  payload jsonb NOT NULL,
  origin UUID,
  PRIMARY KEY (team_id, notification_id)
);

CREATE TABLE epoch_notifications (
  group_id bytea NOT NULL,
  epoch bigint NOT NULL,
  notification_id UUID NOT NULL,
  payload jsonb NOT NULL,
  origin UUID,
  PRIMARY KEY (group_id, epoch, notification_id)
);

CREATE TABLE local_connection_notifications (
  user_id uuid NOT NULL,
  notification_id uuid NOT NULL,
  payload jsonb NOT NULL,
  origin UUID,
  PRIMARY KEY (user_id, notification_id)
);

CREATE TABLE remote_connection_notifications (
  user_domain text NOT NULL,
  user_id uuid NOT NULL,
  notification_id uuid NOT NULL,
  payload jsonb NOT NULL,
  origin UUID,
  PRIMARY KEY (user_domain, user_id, notification_id)
);

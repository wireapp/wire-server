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
  group_id text NOT NULL,
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

-- Epochs
CREATE TABLE epoch_history (
  group_id text NOT NULL,
  epoch bigint NOT NULL,
  user_id UUID NOT NULL,
  client_id bigint NOT NULL,
  superseded_at timestamptz,
  PRIMARY KEY (group_id, epoch, user_id, client_id)
);

CREATE INDEX epoch_history_client ON epoch_history (user_id, client_id);

-- Last Notifications
CREATE TABLE last_user_notifications (
  user_id UUID NOT NULL,
  notification_id UUID NOT NULL,
  PRIMARY KEY (user_id)
);

CREATE TABLE last_client_notifications (
  user_id UUID NOT NULL,
  client_id bigint NOT NULL,
  notification_id UUID NOT NULL,
  PRIMARY KEY (user_id, client_id)
);

CREATE TABLE last_team_notifications (
  team_id UUID NOT NULL,
  notification_id UUID NOT NULL,
  PRIMARY KEY (team_id)
);

CREATE TABLE last_epoch_notifications (
  group_id text NOT NULL,
  epoch bigint NOT NULL,
  notification_id UUID NOT NULL,
  PRIMARY KEY (group_id, epoch)
);

CREATE TABLE last_local_connection_notifications (
  user_id UUID NOT NULL,
  notification_id UUID NOT NULL,
  PRIMARY KEY (user_id)
);

CREATE TABLE last_remote_connection_notifications (
  user_domain text NOT NULL,
  user_id UUID NOT NULL,
  notification_id UUID NOT NULL,
  PRIMARY KEY (user_domain, user_id)
);

-- Acknowledgements
CREATE TABLE user_notification_acks (
  user_id UUID NOT NULL,
  client_id bigint NOT NULL,
  last_notification_id UUID NOT NULL,
  PRIMARY KEY (user_id, client_id)
);

CREATE TABLE client_notification_acks (
  user_id UUID NOT NULL,
  client_id bigint NOT NULL,
  last_notification_id UUID NOT NULL,
  PRIMARY KEY (user_id, client_id)
);

CREATE TABLE team_notification_acks (
  user_id UUID NOT NULL,
  client_id bigint NOT NULL,
  team_id UUID NOT NULL,
  last_notification_id UUID NOT NULL,
  PRIMARY KEY (user_id, client_id, team_id)
);

CREATE TABLE epoch_notification_acks (
  user_id UUID NOT NULL,
  client_id bigint NOT NULL,
  group_id text NOT NULL,
  epoch bigint NOT NULL,
  last_notification_id UUID NOT NULL,
  PRIMARY KEY (user_id, client_id, group_id, epoch)
);

CREATE TABLE local_connection_acks (
  user_id UUID NOT NULL,
  connected_user_id UUID NOT NULL,
  last_notification_id UUID NOT NULL,
  PRIMARY KEY (user_id, connected_user_id)
);

CREATE TABLE remote_connection_acks (
  user_id UUID NOT NULL,
  connected_user_domain text NOT NULL,
  connected_user_id UUID NOT NULL,
  last_notification_id UUID NOT NULL,
  PRIMARY KEY (user_id, connected_user_domain, connected_user_id)
);

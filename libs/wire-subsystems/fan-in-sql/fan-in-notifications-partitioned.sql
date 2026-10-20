-- Used by fan-in-perf only (not a migration). SchemaShape = Partitioned (D).
-- Runs after fan-in-common.sql, in the same transaction, on an empty database.
-- The six *_notifications tables are pg_partman managed partitioned tables.
-- Keep the columns and primary keys in sync with fan-in-notifications-plain.sql.
-- Requires the partman schema (see SetupPartman) and role partman_maintainer.

CREATE TABLE user_notifications (
  user_id UUID NOT NULL,
  notification_id UUID NOT NULL,
  payload jsonb NOT NULL,
  origin UUID,
  PRIMARY KEY (user_id, notification_id)
) PARTITION BY RANGE (notification_id);

CREATE TABLE client_notifications (
  user_id UUID NOT NULL,
  client_id bigint NOT NULL,
  notification_id UUID NOT NULL,
  payload jsonb NOT NULL,
  origin UUID,
  PRIMARY KEY (user_id, client_id, notification_id)
) PARTITION BY RANGE (notification_id);

CREATE TABLE team_notifications (
  team_id UUID NOT NULL,
  notification_id UUID NOT NULL,
  payload jsonb NOT NULL,
  origin UUID,
  PRIMARY KEY (team_id, notification_id)
) PARTITION BY RANGE (notification_id);

CREATE TABLE epoch_notifications (
  group_id bytea NOT NULL,
  epoch bigint NOT NULL,
  notification_id UUID NOT NULL,
  payload jsonb NOT NULL,
  origin UUID,
  PRIMARY KEY (group_id, epoch, notification_id)
) PARTITION BY RANGE (notification_id);

CREATE TABLE local_connection_notifications (
  user_id uuid NOT NULL,
  notification_id uuid NOT NULL,
  payload jsonb NOT NULL,
  origin UUID,
  PRIMARY KEY (user_id, notification_id)
) PARTITION BY RANGE (notification_id);

CREATE TABLE remote_connection_notifications (
  user_domain text NOT NULL,
  user_id uuid NOT NULL,
  notification_id uuid NOT NULL,
  payload jsonb NOT NULL,
  origin UUID,
  PRIMARY KEY (user_domain, user_id, notification_id)
) PARTITION BY RANGE (notification_id);

ALTER TABLE user_notifications OWNER TO partman_maintainer;
ALTER TABLE client_notifications OWNER TO partman_maintainer;
ALTER TABLE team_notifications OWNER TO partman_maintainer;
ALTER TABLE epoch_notifications OWNER TO partman_maintainer;
ALTER TABLE local_connection_notifications OWNER TO partman_maintainer;
ALTER TABLE remote_connection_notifications OWNER TO partman_maintainer;

-- create_parent creates the initial children and the default partition as the
-- session user, and inherit_privileges is off. The bgw runs as
-- partman_maintainer and could not DROP children it does not own ("must be
-- owner"), which rolls back the whole maintenance tick. So create them as
-- partman_maintainer. Plain SET (not LOCAL); reset after the last statement.
SET ROLE partman_maintainer;

SELECT partman.create_parent(p_parent_table := 'public.user_notifications', p_control := 'notification_id', p_interval := '1 minute', p_premake := 10, p_time_encoder := 'partman.uuid7_time_encoder', p_time_decoder := 'partman.uuid7_time_decoder');
SELECT partman.create_parent(p_parent_table := 'public.client_notifications', p_control := 'notification_id', p_interval := '1 minute', p_premake := 10, p_time_encoder := 'partman.uuid7_time_encoder', p_time_decoder := 'partman.uuid7_time_decoder');
SELECT partman.create_parent(p_parent_table := 'public.team_notifications', p_control := 'notification_id', p_interval := '1 minute', p_premake := 10, p_time_encoder := 'partman.uuid7_time_encoder', p_time_decoder := 'partman.uuid7_time_decoder');
SELECT partman.create_parent(p_parent_table := 'public.epoch_notifications', p_control := 'notification_id', p_interval := '1 minute', p_premake := 10, p_time_encoder := 'partman.uuid7_time_encoder', p_time_decoder := 'partman.uuid7_time_decoder');
SELECT partman.create_parent(p_parent_table := 'public.local_connection_notifications', p_control := 'notification_id', p_interval := '1 minute', p_premake := 10, p_time_encoder := 'partman.uuid7_time_encoder', p_time_decoder := 'partman.uuid7_time_decoder');
SELECT partman.create_parent(p_parent_table := 'public.remote_connection_notifications', p_control := 'notification_id', p_interval := '1 minute', p_premake := 10, p_time_encoder := 'partman.uuid7_time_encoder', p_time_decoder := 'partman.uuid7_time_decoder');

UPDATE partman.part_config
SET retention = '5 minutes', retention_keep_table = false, infinite_time_partitions = true
WHERE parent_table IN (
  'public.user_notifications',
  'public.client_notifications',
  'public.team_notifications',
  'public.epoch_notifications',
  'public.local_connection_notifications',
  'public.remote_connection_notifications'
);

RESET ROLE;

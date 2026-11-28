CREATE TABLE user_notifications (
    user_id uuid NOT NULL,
    notification_id uuid NOT NULL,
    payload jsonb NOT NULL,
    origin uuid,
    created_at timestamptz NOT NULL,
    PRIMARY KEY (user_id, notification_id, created_at)
) PARTITION BY RANGE (created_at);

CREATE TABLE client_notifications (
    user_id uuid NOT NULL,
    client_id bigint NOT NULL,
    notification_id uuid NOT NULL,
    payload jsonb NOT NULL,
    origin uuid,
    created_at timestamptz NOT NULL,
    PRIMARY KEY (user_id, client_id, notification_id, created_at)
) PARTITION BY RANGE (created_at);

CREATE TABLE team_notifications (
    team_id uuid NOT NULL,
    notification_id uuid NOT NULL,
    payload jsonb NOT NULL,
    origin uuid,
    created_at timestamptz NOT NULL,
    PRIMARY KEY (team_id, notification_id, created_at)
) PARTITION BY RANGE (created_at);

CREATE TABLE epoch_notifications (
    group_id bytea NOT NULL,
    epoch bigint NOT NULL,
    notification_id uuid NOT NULL,
    payload jsonb NOT NULL,
    origin uuid,
    created_at timestamptz NOT NULL,
    PRIMARY KEY (group_id, epoch, notification_id, created_at)
) PARTITION BY RANGE (created_at);

CREATE TABLE local_connection_notifications (
    user_id uuid NOT NULL,
    notification_id uuid NOT NULL,
    payload jsonb NOT NULL,
    origin uuid,
    created_at timestamptz NOT NULL,
    PRIMARY KEY (user_id, notification_id, created_at)
) PARTITION BY RANGE (created_at);

CREATE TABLE remote_connection_notifications (
    user_domain text NOT NULL,
    user_id uuid NOT NULL,
    notification_id uuid NOT NULL,
    payload jsonb NOT NULL,
    origin uuid,
    created_at timestamptz NOT NULL,
    PRIMARY KEY (user_domain, user_id, notification_id, created_at)
) PARTITION BY RANGE (created_at);

-- Current epoch members are separate from superseded membership history. This
-- keeps NULL/current rows out of a partman default partition and lets old
-- membership history expire by dropping whole partitions.
CREATE TABLE epoch_members (
    group_id bytea NOT NULL,
    epoch bigint NOT NULL,
    user_id uuid NOT NULL,
    client_id bigint NOT NULL,
    PRIMARY KEY (group_id, epoch, user_id, client_id)
);

CREATE TABLE epoch_history (
    group_id bytea NOT NULL,
    epoch bigint NOT NULL,
    user_id uuid NOT NULL,
    client_id bigint NOT NULL,
    superseded_at timestamptz NOT NULL,
    PRIMARY KEY (group_id, epoch, user_id, client_id, superseded_at)
) PARTITION BY RANGE (superseded_at);
CREATE INDEX epoch_history_client ON epoch_history (user_id, client_id, superseded_at);

CREATE TABLE last_user_notifications (
    user_id uuid PRIMARY KEY,
    notification_id uuid NOT NULL
);
CREATE TABLE last_client_notifications (
    user_id uuid NOT NULL,
    client_id bigint NOT NULL,
    notification_id uuid NOT NULL,
    PRIMARY KEY (user_id, client_id)
);
CREATE TABLE last_team_notifications (
    team_id uuid PRIMARY KEY,
    notification_id uuid NOT NULL
);
CREATE TABLE last_epoch_notifications (
    group_id bytea NOT NULL,
    epoch bigint NOT NULL,
    notification_id uuid NOT NULL,
    PRIMARY KEY (group_id, epoch)
);
CREATE TABLE last_local_connection_notifications (
    user_id uuid PRIMARY KEY,
    notification_id uuid NOT NULL
);
CREATE TABLE last_remote_connection_notifications (
    user_domain text NOT NULL,
    user_id uuid NOT NULL,
    notification_id uuid NOT NULL,
    PRIMARY KEY (user_domain, user_id)
);

CREATE TABLE user_notification_acks (
    user_id uuid NOT NULL,
    client_id bigint NOT NULL,
    last_notification_id uuid NOT NULL,
    PRIMARY KEY (user_id, client_id)
);
CREATE TABLE client_notification_acks (
    user_id uuid NOT NULL,
    client_id bigint NOT NULL,
    last_notification_id uuid NOT NULL,
    PRIMARY KEY (user_id, client_id)
);
CREATE TABLE team_notification_acks (
    user_id uuid NOT NULL,
    client_id bigint NOT NULL,
    team_id uuid NOT NULL,
    last_notification_id uuid NOT NULL,
    PRIMARY KEY (user_id, client_id, team_id)
);
CREATE TABLE epoch_notification_acks (
    user_id uuid NOT NULL,
    client_id bigint NOT NULL,
    group_id bytea NOT NULL,
    epoch bigint NOT NULL,
    last_notification_id uuid NOT NULL,
    PRIMARY KEY (user_id, client_id, group_id, epoch)
);
CREATE TABLE local_connection_acks (
    user_id uuid NOT NULL,
    connected_user_id uuid NOT NULL,
    last_notification_id uuid NOT NULL,
    PRIMARY KEY (user_id, connected_user_id)
);
CREATE TABLE remote_connection_acks (
    user_id uuid NOT NULL,
    connected_user_domain text NOT NULL,
    connected_user_id uuid NOT NULL,
    last_notification_id uuid NOT NULL,
    PRIMARY KEY (user_id, connected_user_domain, connected_user_id)
);

ALTER TABLE user_notifications OWNER TO partman_maintainer;
ALTER TABLE client_notifications OWNER TO partman_maintainer;
ALTER TABLE team_notifications OWNER TO partman_maintainer;
ALTER TABLE epoch_notifications OWNER TO partman_maintainer;
ALTER TABLE local_connection_notifications OWNER TO partman_maintainer;
ALTER TABLE remote_connection_notifications OWNER TO partman_maintainer;
ALTER TABLE epoch_history OWNER TO partman_maintainer;

SET ROLE partman_maintainer;
SELECT partman.create_parent(p_parent_table := 'public.user_notifications', p_control := 'created_at', p_interval := '1 day', p_type := 'range', p_premake := 2, p_default_table := false);
SELECT partman.create_parent(p_parent_table := 'public.client_notifications', p_control := 'created_at', p_interval := '1 day', p_type := 'range', p_premake := 2, p_default_table := false);
SELECT partman.create_parent(p_parent_table := 'public.team_notifications', p_control := 'created_at', p_interval := '1 day', p_type := 'range', p_premake := 2, p_default_table := false);
SELECT partman.create_parent(p_parent_table := 'public.epoch_notifications', p_control := 'created_at', p_interval := '1 day', p_type := 'range', p_premake := 2, p_default_table := false);
SELECT partman.create_parent(p_parent_table := 'public.local_connection_notifications', p_control := 'created_at', p_interval := '1 day', p_type := 'range', p_premake := 2, p_default_table := false);
SELECT partman.create_parent(p_parent_table := 'public.remote_connection_notifications', p_control := 'created_at', p_interval := '1 day', p_type := 'range', p_premake := 2, p_default_table := false);
SELECT partman.create_parent(p_parent_table := 'public.epoch_history', p_control := 'superseded_at', p_interval := '1 day', p_type := 'range', p_premake := 2, p_default_table := false);

RESET ROLE;
UPDATE partman.part_config
SET retention = '28 days',
    retention_keep_table = false,
    infinite_time_partitions = true
WHERE parent_table IN (
    'public.user_notifications',
    'public.client_notifications',
    'public.team_notifications',
    'public.epoch_notifications',
    'public.local_connection_notifications',
    'public.remote_connection_notifications',
    'public.epoch_history'
);
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA public TO "wire-server";

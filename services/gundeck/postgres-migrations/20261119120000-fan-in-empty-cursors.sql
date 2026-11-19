-- Empty streams need a durable starting boundary too. These rows carry no
-- event payload and expire with notifications, allowing ordinary gap detection.
CREATE TABLE fan_in_empty_cursors (
    cursor_id uuid NOT NULL,
    created_at timestamptz NOT NULL,
    PRIMARY KEY (cursor_id, created_at)
) PARTITION BY RANGE (created_at);
ALTER TABLE fan_in_empty_cursors OWNER TO partman_maintainer;
SET ROLE partman_maintainer;
SELECT partman.create_parent(p_parent_table := 'public.fan_in_empty_cursors', p_control := 'created_at', p_interval := '1 day', p_type := 'range', p_premake := 2, p_default_table := false);
RESET ROLE;
UPDATE partman.part_config
SET retention = '28 days', retention_keep_table = false, infinite_time_partitions = true
WHERE parent_table = 'public.fan_in_empty_cursors';
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA public TO "wire-server";

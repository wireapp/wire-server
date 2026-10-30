-- Team member tables (the member half of the Cassandra team store).
--
-- Encodings mirror the Cassandra schema (cassandra-schema.cql: team_member,
-- user_team, billing_team_member, team_admin):
--
--   * Permissions  -> two int8 columns (perms_self, perms_copy) via the
--     self/copy permission bitmask pair (Cassandra frozen<permissions> UDT)
--   * UserLegalHoldStatus -> text with the wire JSON/ swagger tag values
--     (disabled|pending|enabled|no_consent)
--
-- NOTE: "user" is a reserved word in PostgreSQL; user-facing columns are
-- named user_id.
--
-- perms_write_time maintains Cassandra's writetime(perms): set to now() on
-- direct permission writes and carried over from Cassandra by the member
-- migration copy loop; served as TeamMemberInfo.permissionsWriteTime.

CREATE TABLE IF NOT EXISTS team_member (
    team             uuid NOT NULL,
    user_id          uuid NOT NULL,
    perms_self       int8 NOT NULL,
    perms_copy       int8 NOT NULL,
    invited_by       uuid,
    invited_at       timestamptz,
    legalhold_status text,
    perms_write_time timestamptz NOT NULL DEFAULT now(),
    PRIMARY KEY (team, user_id)
);

CREATE INDEX IF NOT EXISTS team_member_user_idx ON team_member (user_id);

CREATE TABLE IF NOT EXISTS user_team (
    user_id uuid NOT NULL,
    team    uuid NOT NULL,
    PRIMARY KEY (user_id, team)
);

CREATE INDEX IF NOT EXISTS user_team_team_idx ON user_team (team);

CREATE TABLE IF NOT EXISTS billing_team_member (
    team    uuid NOT NULL,
    user_id uuid NOT NULL,
    PRIMARY KEY (team, user_id)
);

CREATE TABLE IF NOT EXISTS team_admin (
    team    uuid NOT NULL,
    user_id uuid NOT NULL,
    PRIMARY KEY (team, user_id)
);

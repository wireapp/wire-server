-- WPB-22960-pre: TeamStore (team + team_member + auxiliary membership tables).
--
-- Encodings mirror the Cassandra schema (cassandra-schema.cql: team, team_member,
-- user_team, billing_team_member, team_admin):
--
--   * Permissions  -> two int8 columns (perms_self, perms_copy) via permsToInt
--   * enum domains -> text with the wire JSON/ swagger tag values
--     (TeamStatus: active|pending_delete|deleted|suspended|pending_active,
--      TeamBinding: binding|non_binding,
--      TeamSearchVisibility: standard|no_name_outside_team,
--      UserLegalHoldStatus: disabled|pending|enabled|no_consent)
--   * Icon         -> text, 'default' or the asset key
--
-- Every column whose Cassandra writetime is consumed is mirrored by a nullable
-- @*_time@ column holding that writetime.  The two consumers of writetimes
-- (TeamData.tdStatusTime from writetime(status) and TeamCreationTime from
-- writetime(binding)) read @status_time@ and @binding_time@; like Cassandra's
-- writetime function, those columns are NULL when the corresponding Cassandra
-- column has no writetime.  They are carried over from Cassandra by the team
-- migration copy loop and maintained by the dual-write leg via now().
--
-- NOTE: "user" is a reserved word in PostgreSQL; user-facing columns are named
-- user_id.

CREATE TABLE IF NOT EXISTS team (
    team          uuid PRIMARY KEY,
    creator       uuid NOT NULL,
    name          text NOT NULL,
    icon          text NOT NULL,
    icon_key      text,
    splash_screen text,
    deleted       boolean NOT NULL DEFAULT false,
    status        text NOT NULL,
    status_time   timestamptz,
    binding       text NOT NULL,
    binding_time  timestamptz,
    search_visibility text
);

CREATE TABLE IF NOT EXISTS team_member (
    team             uuid NOT NULL,
    user_id          uuid NOT NULL,
    perms_self       int8 NOT NULL,
    perms_copy       int8 NOT NULL,
    invited_by       uuid,
    invited_at       timestamptz,
    legalhold_status text,
    perms_write_time       timestamptz NOT NULL,
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

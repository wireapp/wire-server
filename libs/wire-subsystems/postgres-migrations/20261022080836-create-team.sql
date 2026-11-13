-- Team metadata (the `team` table of the Cassandra team store).
--
-- Encodings mirror the Cassandra schema (cassandra-schema.cql: team):
--
--   * enum domains -> text with the wire JSON/ swagger tag values
--     (TeamStatus: active|pending_delete|deleted|suspended|pending_active,
--      TeamBinding: binding|non_binding,
--      TeamSearchVisibility: standard|no-name-outside-team)
--
-- Every column that Cassandra exposes a writetime for is mirrored by a
-- nullable @*_time@ column holding that writetime.  The two consumers of
-- writetimes (TeamData.tdStatusTime from writetime(status) and
-- TeamCreationTime from writetime(binding)) read @status_time@ and
-- @binding_time@; like Cassandra's writetime function, those columns are NULL
-- when the corresponding Cassandra column has no writetime.

CREATE TABLE IF NOT EXISTS team (
    team          uuid PRIMARY KEY,
    creator       uuid NOT NULL,
    name          text NOT NULL,
    name_time     timestamptz,
    icon          text NOT NULL,
    icon_time     timestamptz,
    icon_key      text,
    icon_key_time timestamptz,
    splash_screen text,
    splash_screen_time timestamptz,
    deleted       boolean NOT NULL DEFAULT false,
    status        text NOT NULL,
    status_time   timestamptz,
    binding       text NOT NULL,
    binding_time  timestamptz,
    search_visibility text,
    search_visibility_time timestamptz
);

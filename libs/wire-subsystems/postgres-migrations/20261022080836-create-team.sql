-- Team metadata (the `team` table of the Cassandra team store).
--
-- Encodings mirror the Cassandra schema (cassandra-schema.cql: team):
--
--   * enum domains -> text with the wire JSON/ swagger tag values
--     (TeamStatus: active|pending_delete|deleted|suspended|pending_active,
--      TeamBinding: binding|non_binding,
--      TeamSearchVisibility: standard|no-name-outside-team)
--
-- Cassandra writetimes are not reproducible in Postgres; the two consumers of
-- writetimes (TeamData.tdStatusTime from writetime(status) and
-- TeamCreationTime from writetime(binding)) read maintained timestamptz columns
-- instead (status_time, binding_time), set at insert/update time and carried
-- over from Cassandra by the team migration copy loop.

CREATE TABLE IF NOT EXISTS team (
    team          uuid PRIMARY KEY,
    creator       uuid NOT NULL,
    name          text NOT NULL,
    icon          text NOT NULL,
    icon_key      text,
    splash_screen text,
    deleted       boolean NOT NULL DEFAULT false,
    status        text NOT NULL,
    status_time   timestamptz NOT NULL DEFAULT now(),
    binding       text NOT NULL,
    binding_time  timestamptz NOT NULL DEFAULT now(),
    search_visibility text
);

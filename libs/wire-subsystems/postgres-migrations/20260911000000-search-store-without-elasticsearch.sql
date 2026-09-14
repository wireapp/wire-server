-- Stores the ICU-transliterated, lowercased display name
-- (see 'Wire.UserSearch.Normalize.normalized').  Required for
-- case/diacritic-insensitive prefix search of user names, which plain
-- lower() cannot provide ("Björn" -> "bjorn").
ALTER TABLE wire_user ADD COLUMN name_normalized text;

-- Prefix search indexes (LIKE 'abc%', no pg_trgm needed).
CREATE INDEX wire_user_name_normalized_pattern_idx ON wire_user (name_normalized text_pattern_ops);
CREATE INDEX wire_user_lower_handle_pattern_idx ON wire_user (lower(handle) text_pattern_ops);

CREATE INDEX wire_user_team_idx ON wire_user (team);

-- Replaces the per-user search_visibility_inbound field formerly
-- denormalized into the ElasticSearch user documents.  One row per team;
-- a missing row means 'SearchableByOwnTeam'.
CREATE TABLE team_search_visibility (
  team uuid PRIMARY KEY,
  search_visibility_inbound integer NOT NULL
);

-- Deployment note: rows created before this migration have a NULL
-- name_normalized.  Run `brig-index backfill-normalized-names` once before
-- switching user search over to Postgres, otherwise name search misses
-- every pre-migration user (handle/email search are unaffected).

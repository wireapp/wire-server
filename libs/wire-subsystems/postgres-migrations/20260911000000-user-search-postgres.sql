-- Stores the ICU-transliterated, lowercased display name
-- (see 'Wire.UserSearch.Normalize.normalized').  Required for
-- case/diacritic-insensitive prefix search of user names, which plain
-- lower() cannot provide ("Björn" -> "bjorn").
ALTER TABLE wire_user ADD COLUMN name_normalized text;

-- The three CREATE INDEX CONCURRENTLY statements for this migration live in
-- 20260911000001-user-search-postgres-indexes.sql: they cannot run inside a
-- transaction and are registered in 'Wire.PostgresMigrations.nonTransactionMigrations'.

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

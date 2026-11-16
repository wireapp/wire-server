-- WPB-28377: absolute TTL deadline for gundeck presences.
--
-- Redis expired all presences of a user 7 days after that user's most recent
-- registration (every add refreshed the key TTL).  gundeck mirrors this by
-- setting `expires_at` of all of the user's presences to now() + 7 days on
-- every add, ignores expired rows on read and deletes them on the next add.
-- Expired rows of users who never register again are deleted by the optional
-- pg_cron job (see 20261116100100-presence-ttl-cleanup.sql).
ALTER TABLE presence ADD COLUMN IF NOT EXISTS expires_at timestamptz;

-- backfill rows written before this column existed
UPDATE presence SET expires_at = created_at + interval '7 days' WHERE expires_at IS NULL;

-- the default only matters for writers that do not set the column themselves
ALTER TABLE presence ALTER COLUMN expires_at SET DEFAULT (now() + interval '7 days');
ALTER TABLE presence ALTER COLUMN expires_at SET NOT NULL;

-- index for TTL deletes like `DELETE ... WHERE expires_at < now()`
CREATE INDEX IF NOT EXISTS presence_expires_at_idx ON presence (expires_at);

-- superseded by presence_expires_at_idx
DROP INDEX IF EXISTS presence_created_at_idx;

-- The pg_cron job 'expire-team-invitations' (20261030090000) deletes expired
-- invitations with an unbatched full-table predicate; team_invitation fills
-- fast (one row per invite), so index the delete predicate.
CREATE INDEX IF NOT EXISTS team_invitation_expires_at_idx ON team_invitation (expires_at);

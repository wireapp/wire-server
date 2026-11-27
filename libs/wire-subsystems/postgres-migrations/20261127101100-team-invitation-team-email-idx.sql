-- Supports the live-invitation lookup for pending SCIM users
-- (team_invitation.team = ? AND team_invitation.email = ?).
CREATE INDEX CONCURRENTLY IF NOT EXISTS team_invitation_team_email_idx ON team_invitation (team, email);

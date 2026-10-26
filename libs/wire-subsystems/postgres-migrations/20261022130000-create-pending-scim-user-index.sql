-- PendingInvitation = 4, ManagedByScim = 1. Cover the fields needed to check
-- invitation expiration without fetching complete user records or assets.
CREATE INDEX CONCURRENTLY IF NOT EXISTS wire_user_pending_scim_users_idx
ON wire_user (team) INCLUDE (id, email)
WHERE managed_by = 1 AND account_status = 4 AND activated = true AND email IS NOT NULL;

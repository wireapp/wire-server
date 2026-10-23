CREATE INDEX CONCURRENTLY IF NOT EXISTS wire_user_scim_users_idx ON wire_user (team, id) WHERE managed_by = 1;

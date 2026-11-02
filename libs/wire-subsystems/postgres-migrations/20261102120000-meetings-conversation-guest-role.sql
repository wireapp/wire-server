-- WPB-27191: meeting conversations must admit temporary guests. Brig ephemeral
-- users are identity-less, so galley's access-role heuristic ('ensureAccessRole'
-- via brig /i/users with includeUsersWithoutIdentity=false) treats them as
-- guests and denies the join unless GuestAccessRole is stored. New meetings are
-- created with GuestAccessRole (MeetingsSubsystem createMeetingImpl); backfill
-- existing rows so their join links keep working for temp guests.
-- Mirrors the PG-only backfill of 20260819120000-meetings-conversation-access.sql
-- (WPB-28155). Residual gap: meeting conversations still living only in
-- Cassandra are copied to Postgres afterwards with their pre-fix roles
-- verbatim (the copier does not add GuestAccessRole); re-run this DML as a
-- reconciliation pass at conversation-store cutover.
-- NULL-roles rows are skipped deliberately: they fall back to defRole on read
-- (maybeRole) and are not rewritten into a made-up default.
-- AccessRoleV2 ints (accessRoleToInt32): team_member=1, non_team_member=2,
-- guest=3, service=4. GroupConvType ints (fromEnum): group=0, channel=1, meeting=2.
UPDATE conversation
SET access_roles_v2 = array_append(access_roles_v2, 3)
WHERE group_conv_type = 2
  AND access_roles_v2 IS NOT NULL
  AND NOT (3 = ANY(access_roles_v2));

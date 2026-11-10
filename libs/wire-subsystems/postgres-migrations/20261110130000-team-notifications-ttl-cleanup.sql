-- WPB-22958: delete expired team_notifications rows
-- (ADR 0005: pg_cron for TTL deletions), replacing the externally-provisioned
-- purge job note in 20261026122128-team-notifications.sql.
-- Hourly at :10; volume (member-join events, 28d TTL) is far below the
-- batching threshold of ADR-0005, so a single unbounded DELETE is used.
-- Requires the operator to enable pg_cron (shared_preload_libraries + CREATE EXTENSION);
-- the migration only schedules the job and must not fail boot without it.
DO $mig$
BEGIN
  IF EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'pg_cron') THEN
    BEGIN
      PERFORM cron.schedule(
        'expire-team-notifications',
        '10 * * * *',
        $$DELETE FROM public.team_notifications WHERE expires_at < now()$$
      );
    EXCEPTION WHEN OTHERS THEN
      RAISE WARNING 'could not schedule expire-team-notifications: % (see ADR 0005-use-pg-cron-for-ttl-deletions; after fixing permissions/pg_cron version, re-run this DO block)', SQLERRM;
    END;
  ELSE
    RAISE WARNING 'pg_cron not installed: expiry cleanup for team_notifications NOT scheduled (see ADR 0005-use-pg-cron-for-ttl-deletions)';
  END IF;
END
$mig$;

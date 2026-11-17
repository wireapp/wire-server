-- WPB-28377: periodically delete expired presence rows with pg_cron.
--
-- gundeck itself only deletes a user's expired presences when that user
-- registers a new presence; this job also removes rows of users who never
-- come back.  It is optional: expired rows are ignored on read anyway.
--
-- Requires the operator to enable pg_cron (shared_preload_libraries + CREATE
-- EXTENSION) in this database, i.e. `cron.database_name` must point to it;
-- the migration only schedules the job and must not fail without it.  The job
-- is explicitly scheduled to run in the database this migration is applied to.
DO $mig$
BEGIN
  IF EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'pg_cron') THEN
    BEGIN
      PERFORM cron.schedule_in_database(
        'expire-presence',
        '5 * * * *',
        $$DELETE FROM public.presence WHERE expires_at < now()$$,
        current_database()
      );
    EXCEPTION WHEN OTHERS THEN
      RAISE WARNING 'could not schedule expire-presence: % (after fixing permissions/pg_cron version, re-run this DO block)', SQLERRM;
    END;
  ELSE
    RAISE WARNING 'pg_cron not installed: expiry cleanup for presence NOT scheduled; expired presences are only deleted when their user registers a new presence';
  END IF;
END
$mig$;

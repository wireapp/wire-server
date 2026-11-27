-- WPB-29577: delete expired vcodes and vcodes_throttle rows (ADR 0005: pg_cron
-- for TTL deletions). Verification codes are short-lived, so both jobs run
-- every 5 minutes.
-- Requires the operator to enable pg_cron (shared_preload_libraries + CREATE EXTENSION);
-- the migration only schedules the jobs and must not fail boot without it.
DO $mig$
BEGIN
  IF EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'pg_cron') THEN
    BEGIN
      PERFORM cron.schedule(
        'expire-vcodes',
        '*/5 * * * *',
        $$DELETE FROM vcodes WHERE expires_at < now()$$
      );
      PERFORM cron.schedule(
        'expire-vcodes-throttle',
        '*/5 * * * *',
        $$DELETE FROM vcodes_throttle WHERE expires_at < now()$$
      );
    EXCEPTION WHEN OTHERS THEN
      RAISE WARNING 'could not schedule expire-vcodes/expire-vcodes-throttle: % (see ADR 0005-use-pg-cron-for-ttl-deletions; after fixing permissions/pg_cron version, re-run this DO block)', SQLERRM;
    END;
  ELSE
    RAISE WARNING 'pg_cron not installed: expiry cleanup for vcodes/vcodes_throttle NOT scheduled (see ADR 0005-use-pg-cron-for-ttl-deletions)';
  END IF;
END
$mig$;

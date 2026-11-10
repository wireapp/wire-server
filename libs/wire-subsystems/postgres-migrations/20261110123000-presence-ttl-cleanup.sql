-- WPB-28377: delete expired presence rows (ADR 0005: pg_cron for TTL deletions),
-- replacing the per-replica Haskell cleanup loop.
-- Requires the operator to enable pg_cron (shared_preload_libraries + CREATE EXTENSION);
-- the migration only schedules the job and must not fail boot without it.
DO $mig$
BEGIN
  IF EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'pg_cron') THEN
    BEGIN
      PERFORM cron.schedule(
        'expire-presence',
        '5 * * * *',
        $$DELETE FROM public.presence WHERE expires_at < now()$$
      );
    EXCEPTION WHEN OTHERS THEN
      RAISE WARNING 'could not schedule expire-presence: % (see ADR 0005-use-pg-cron-for-ttl-deletions; after fixing permissions/pg_cron version, re-run this DO block)', SQLERRM;
    END;
  ELSE
    RAISE WARNING 'pg_cron not installed: expiry cleanup for presence NOT scheduled (see ADR 0005-use-pg-cron-for-ttl-deletions)';
  END IF;
END
$mig$;

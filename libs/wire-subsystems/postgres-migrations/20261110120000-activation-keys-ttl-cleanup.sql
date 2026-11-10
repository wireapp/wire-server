-- WPB-22970: delete expired activation_keys rows (ADR 0005: pg_cron for TTL
-- deletions). The job uses the batched ctid form so a single run deletes at
-- most 10000 rows and never holds a long transaction on the table; repeated
-- runs converge on the full set.
-- Requires the operator to enable pg_cron (shared_preload_libraries + CREATE EXTENSION);
-- the migration only schedules the job and must not fail boot without it.
DO $mig$
BEGIN
  IF EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'pg_cron') THEN
    BEGIN
      PERFORM cron.schedule(
        'expire-activation-keys',
        '0 3 * * *',
        $$DELETE FROM activation_keys WHERE ctid IN (SELECT ctid FROM activation_keys WHERE expires_at <= now() LIMIT 10000)$$
      );
    EXCEPTION WHEN OTHERS THEN
      RAISE WARNING 'could not schedule expire-activation-keys: % (see ADR 0005-use-pg-cron-for-ttl-deletions; after fixing permissions/pg_cron version, re-run this DO block)', SQLERRM;
    END;
  ELSE
    RAISE WARNING 'pg_cron not installed: expiry cleanup for activation_keys NOT scheduled (see ADR 0005-use-pg-cron-for-ttl-deletions)';
  END IF;
END
$mig$;

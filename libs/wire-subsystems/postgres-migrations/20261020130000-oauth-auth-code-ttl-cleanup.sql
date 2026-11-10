-- WPB-22966: delete expired oauth_auth_code rows (ADR 0005: pg_cron for TTL deletions).
-- Authorization codes are short-lived (table-level TTL); a daily sweep is
-- sufficient because every read filters on expires_at.
-- Requires the operator to enable pg_cron (shared_preload_libraries + CREATE EXTENSION);
-- the migration only schedules the job and must not fail boot without it.
DO $mig$
BEGIN
  IF EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'pg_cron') THEN
    BEGIN
      PERFORM cron.schedule(
        'expire-oauth-auth-code',
        '0 4 * * *',
        $$DELETE FROM public.oauth_auth_code WHERE expires_at < now()$$
      );
    EXCEPTION WHEN OTHERS THEN
      RAISE WARNING 'could not schedule expire-oauth-auth-code: % (see ADR 0005-use-pg-cron-for-ttl-deletions; after fixing permissions/pg_cron version, re-run this DO block)', SQLERRM;
    END;
  ELSE
    RAISE WARNING 'pg_cron not installed: expiry cleanup for oauth_auth_code NOT scheduled (see ADR 0005-use-pg-cron-for-ttl-deletions)';
  END IF;
END
$mig$;

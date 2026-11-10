-- WPB-29578: delete expired user_cookies rows (ADR 0005: pg_cron for TTL
-- deletions). Sessions are auth-relevant, so the purge runs hourly; the
-- expression index serves the purge scan (the existing
-- user_cookies_expires_idx leads with user_id and cannot).
-- Requires the operator to enable pg_cron (shared_preload_libraries + CREATE EXTENSION);
-- the migration only schedules the job and must not fail boot without it.
CREATE INDEX IF NOT EXISTS user_cookies_expires_at_idx ON user_cookies (expires_at);

DO $mig$
BEGIN
  IF EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'pg_cron') THEN
    BEGIN
      PERFORM cron.schedule(
        'expire-user-cookies',
        '5 * * * *',
        $$DELETE FROM user_cookies WHERE expires_at < now() OR invalid_after < now()$$
      );
    EXCEPTION WHEN OTHERS THEN
      RAISE WARNING 'could not schedule expire-user-cookies: % (see ADR 0005-use-pg-cron-for-ttl-deletions; after fixing permissions/pg_cron version, re-run this DO block)', SQLERRM;
    END;
  ELSE
    RAISE WARNING 'pg_cron not installed: expiry cleanup for user_cookies NOT scheduled (see ADR 0005-use-pg-cron-for-ttl-deletions)';
  END IF;
END
$mig$;

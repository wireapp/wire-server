-- ADR 0005: TTL-based deletions are pg_cron jobs.  Registration is guarded so
-- deployments without pg_cron (dev/test) still migrate; the operator enables
-- the extension and the job surfaces on the next fresh migration.
DO $inv$
BEGIN
  BEGIN
    EXECUTE 'CREATE EXTENSION IF NOT EXISTS pg_cron';
  EXCEPTION WHEN OTHERS THEN
    RAISE WARNING 'pg_cron not enabled (superuser + shared_preload_libraries required): %', SQLERRM;
  END;
  IF EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'pg_cron') THEN
    BEGIN
      PERFORM cron.schedule('expire-team-invitations', '0 4 * * *',
                            $job$DELETE FROM public.team_invitation WHERE expires_at < now()$job$);
    EXCEPTION WHEN OTHERS THEN
      RAISE WARNING 'could not schedule expire-team-invitations: %', SQLERRM;
    END;
  ELSE
    RAISE WARNING 'schedule expire-team-invitations manually: DELETE FROM public.team_invitation WHERE expires_at < now()';
  END IF;
END $inv$;

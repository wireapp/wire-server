-- WPB-22971: delete expired notifications / notification_payload rows
-- (ADR 0005: pg_cron for TTL deletions). Both tables are high-volume, so the
-- deletes run in bounded batches (ctid) per ADR 0005's high-volume rule
-- instead of one unbounded DELETE.
-- Requires the operator to enable pg_cron (shared_preload_libraries + CREATE EXTENSION);
-- the migration only schedules the job and must not fail boot without it.
DO $mig$
BEGIN
  IF EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'pg_cron') THEN
    BEGIN
      PERFORM cron.schedule(
        'expire-notifications',
        '0 * * * *',
        $$DELETE FROM public.notifications WHERE ctid IN (SELECT ctid FROM public.notifications WHERE expires_at < now() LIMIT 10000)$$
      );
    EXCEPTION WHEN OTHERS THEN
      RAISE WARNING 'could not schedule expire-notifications: % (see ADR 0005-use-pg-cron-for-ttl-deletions; after fixing permissions/pg_cron version, re-run this DO block)', SQLERRM;
    END;
    BEGIN
      PERFORM cron.schedule(
        'expire-notification-payload',
        '0 * * * *',
        $$DELETE FROM public.notification_payload WHERE ctid IN (SELECT ctid FROM public.notification_payload WHERE expires_at < now() LIMIT 10000)$$
      );
    EXCEPTION WHEN OTHERS THEN
      RAISE WARNING 'could not schedule expire-notification-payload: % (see ADR 0005-use-pg-cron-for-ttl-deletions; after fixing permissions/pg_cron version, re-run this DO block)', SQLERRM;
    END;
  ELSE
    RAISE WARNING 'pg_cron not installed: expiry cleanup for notifications / notification_payload NOT scheduled (see ADR 0005-use-pg-cron-for-ttl-deletions)';
  END IF;
END
$mig$;

-- meetings-externally-managed-by: lifecycle owner + external calendar references.
-- Nullable text: absent = wire-managed; no enum (values are a client convention).
ALTER TABLE meetings ADD COLUMN externally_managed_by text;
ALTER TABLE meetings ADD COLUMN externally_calendar_event_id text;
ALTER TABLE meetings ADD COLUMN externally_calendar_id text;

-- meetings-calendar-outlook: outlook lifecycle owner + external calendar references.
ALTER TYPE meeting_manager ADD VALUE IF NOT EXISTS 'outlook';
ALTER TABLE meetings ADD COLUMN IF NOT EXISTS calendar_event_id text;
ALTER TABLE meetings ADD COLUMN IF NOT EXISTS calendar_id text;

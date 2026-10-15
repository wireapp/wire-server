-- meetings-managed-by: lifecycle owner of a meeting. 'google_calendar' meetings are
-- only mutable through the calendar integration (OAuth callers).
CREATE TYPE meeting_manager AS ENUM ('wire', 'google_calendar');
ALTER TABLE meetings ADD COLUMN managed_by meeting_manager NOT NULL DEFAULT 'wire';

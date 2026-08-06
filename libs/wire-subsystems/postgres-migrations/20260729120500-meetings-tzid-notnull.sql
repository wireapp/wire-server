-- Backfill the (nullable) meetings.tzid column added by the preceding migration
-- with the default legacy time zone, then make it NOT NULL. There is no column
-- DEFAULT: the application always supplies the value on create (V17) or via the
-- galley meetings.legacyTimeZone config on the legacy (< V17) create path.
UPDATE meetings SET tzid = 'Europe/Berlin' WHERE tzid IS NULL;
ALTER TABLE meetings ALTER COLUMN tzid SET NOT NULL;

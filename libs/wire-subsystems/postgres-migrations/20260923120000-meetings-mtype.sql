-- WPB-28985: meeting type (immediate | scheduled), exposed at API version V19.
-- The DEFAULT backfills all pre-existing rows to 'scheduled'.
CREATE TYPE meeting_type AS ENUM ('immediate', 'scheduled');
ALTER TABLE meetings ADD COLUMN mtype meeting_type NOT NULL DEFAULT 'scheduled';

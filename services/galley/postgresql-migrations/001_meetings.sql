-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

-- Migration: Add meetings table to PostgreSQL
-- Description: Creates the meetings table with all required fields, indices, and constraints

CREATE TABLE IF NOT EXISTS meetings (
    -- Meeting identification
    id uuid NOT NULL,
    domain text NOT NULL,

    -- Meeting metadata
    title text NOT NULL,
    creator uuid NOT NULL,
    creator_domain text NOT NULL,

    -- Scheduling information
    start_date timestamptz NOT NULL,
    end_date timestamptz NOT NULL,
    recurrence jsonb,

    -- Associated conversation
    conversation_id uuid NOT NULL,
    conversation_domain text NOT NULL,

    -- Invitations
    invited_emails text[] NOT NULL DEFAULT '{}',

    -- Feature flags
    trial boolean NOT NULL DEFAULT false,

    -- Timestamps
    created_at timestamptz NOT NULL DEFAULT NOW(),

    -- Primary key
    PRIMARY KEY (domain, id)
);

-- Indices for performance

-- Index for looking up meetings by creator (user)
CREATE INDEX IF NOT EXISTS idx_meetings_creator
    ON meetings(creator);

-- Index for looking up meetings by conversation
CREATE INDEX IF NOT EXISTS idx_meetings_conversation
    ON meetings(conversation_domain, conversation_id);

-- Index for cleanup queries (finding old meetings)
CREATE INDEX IF NOT EXISTS idx_meetings_end_date
    ON meetings(end_date);

-- Index for querying meetings within a time range
CREATE INDEX IF NOT EXISTS idx_meetings_start_date
    ON meetings(start_date);

-- Constraints

-- Ensure end_date is after start_date
ALTER TABLE meetings
    ADD CONSTRAINT meetings_valid_date_range
    CHECK (end_date > start_date);

-- Ensure title is not empty
ALTER TABLE meetings
    ADD CONSTRAINT meetings_title_not_empty
    CHECK (length(trim(title)) > 0);

-- Ensure title is not too long (reasonable limit)
ALTER TABLE meetings
    ADD CONSTRAINT meetings_title_length
    CHECK (length(title) <= 256);

-- Comments for documentation
COMMENT ON TABLE meetings IS 'Scheduled meetings with email invitations';
COMMENT ON COLUMN meetings.id IS 'Unique meeting identifier (UUID)';
COMMENT ON COLUMN meetings.domain IS 'Federation domain for the meeting';
COMMENT ON COLUMN meetings.title IS 'Meeting title/subject';
COMMENT ON COLUMN meetings.creator IS 'User ID who created the meeting';
COMMENT ON COLUMN meetings.creator_domain IS 'Domain of the user who created the meeting';
COMMENT ON COLUMN meetings.start_date IS 'Meeting start time';
COMMENT ON COLUMN meetings.end_date IS 'Meeting end time';
COMMENT ON COLUMN meetings.recurrence IS 'Optional recurring schedule information (JSON)';
COMMENT ON COLUMN meetings.conversation_id IS 'Associated conversation ID';
COMMENT ON COLUMN meetings.conversation_domain IS 'Domain of the associated conversation';
COMMENT ON COLUMN meetings.invited_emails IS 'Array of email addresses invited to the meeting';
COMMENT ON COLUMN meetings.trial IS 'Whether this meeting is created under a trial account';
COMMENT ON COLUMN meetings.created_at IS 'Timestamp when the meeting was created';

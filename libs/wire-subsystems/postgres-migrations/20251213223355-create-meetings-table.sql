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
    id uuid NOT NULL,
    title text NOT NULL,
    creator uuid NOT NULL,
    start_time timestamptz NOT NULL,
    end_time timestamptz NOT NULL,
    recurrence_frequency text,
    recurrence_interval integer,
    recurrence_until timestamptz,
    conversation_id uuid NOT NULL,
    invited_emails text[] NOT NULL DEFAULT '{}',
    trial boolean NOT NULL DEFAULT false,
    created_at timestamptz NOT NULL DEFAULT NOW(),
    updated_at timestamptz NOT NULL DEFAULT NOW(),
    PRIMARY KEY (id),
    CONSTRAINT meetings_valid_time_range CHECK (end_time > start_time),
    CONSTRAINT meetings_title_not_empty CHECK (length(trim(title)) > 0),
    CONSTRAINT meetings_title_length CHECK (length(title) <= 256)
);

-- Indices for performance

-- Index for looking up meetings by creator (user)
CREATE INDEX IF NOT EXISTS idx_meetings_creator
    ON meetings(creator);

-- Index for looking up meetings by conversation
CREATE INDEX IF NOT EXISTS idx_meetings_conversation
    ON meetings(conversation_id);

-- Index for cleanup queries (finding old meetings)
CREATE INDEX IF NOT EXISTS idx_meetings_end_time
    ON meetings(end_time);

-- Index for querying meetings within a time range
CREATE INDEX IF NOT EXISTS idx_meetings_start_time
    ON meetings(start_time);

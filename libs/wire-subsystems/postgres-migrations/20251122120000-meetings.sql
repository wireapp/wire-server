-- Wire Meetings table
-- Create meetings table for storing scheduled meetings

CREATE TABLE IF NOT EXISTS meetings (
    id UUID NOT NULL,
    domain TEXT NOT NULL,
    title TEXT NOT NULL,
    creator UUID NOT NULL,
    creator_domain TEXT NOT NULL,
    start_date TIMESTAMPTZ NOT NULL,
    end_date TIMESTAMPTZ NOT NULL,
    recurrence JSONB,
    conversation_id UUID NOT NULL,
    conversation_domain TEXT NOT NULL,
    invited_emails TEXT[] DEFAULT '{}',
    trial BOOLEAN DEFAULT FALSE,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW(),
    PRIMARY KEY (domain, id)
);

-- Indexes for common queries
CREATE INDEX IF NOT EXISTS idx_meetings_creator ON meetings(creator);
CREATE INDEX IF NOT EXISTS idx_meetings_conversation ON meetings(conversation_id, conversation_domain);
CREATE INDEX IF NOT EXISTS idx_meetings_start_date ON meetings(start_date);
CREATE INDEX IF NOT EXISTS idx_meetings_end_date ON meetings(end_date);

-- Migration: Add a minimal scheduled-jobs catalog for app-level lookup
-- Description: Stores the metadata we need to find and manage scheduled jobs
--              while Arbiter owns runtime execution state.

CREATE TABLE IF NOT EXISTS scheduled_jobs (
    id uuid NOT NULL,                  -- app-level job id
    kind int NOT NULL,                 -- maps to a Haskell sum type
    team_id uuid NOT NULL,             -- team scope for teardown and lookup
    conversation_id uuid,              -- optional conversation scope for later lookups
    scheduled_for timestamptz NOT NULL, -- when the job should run
    PRIMARY KEY (id)
);

-- Find the next due jobs quickly.
CREATE INDEX IF NOT EXISTS idx_scheduled_jobs_scheduled_for
    ON scheduled_jobs (scheduled_for);

-- Find jobs by family.
CREATE INDEX IF NOT EXISTS idx_scheduled_jobs_kind
    ON scheduled_jobs (kind);

-- Find jobs for a team, optionally narrowed by family.
CREATE INDEX IF NOT EXISTS idx_scheduled_jobs_team_kind
    ON scheduled_jobs (team_id, kind);

-- Find jobs scoped to a conversation.
CREATE INDEX IF NOT EXISTS idx_scheduled_jobs_conversation_id
    ON scheduled_jobs (conversation_id);

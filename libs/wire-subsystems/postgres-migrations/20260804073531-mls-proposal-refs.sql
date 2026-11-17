CREATE TABLE IF NOT EXISTS mls_proposal_refs (
  group_id   bytea      NOT NULL,
  epoch      int8       NOT NULL,
  ref        bytea      NOT NULL,
  origin     int4,
  proposal   bytea      NOT NULL,
  expires_at timestamptz NOT NULL,
  PRIMARY KEY (group_id, epoch, ref)
);

-- Lookups by (group_id, epoch) and deletes by group_id use the primary key.
-- This index serves the TTL cleanup `DELETE ... WHERE expires_at < now()`.
CREATE INDEX IF NOT EXISTS mls_proposal_refs_expires_at_idx
  ON mls_proposal_refs (expires_at);

CREATE TABLE IF NOT EXISTS mls_proposal_refs (
  group_id   bytea      NOT NULL,
  epoch      int8       NOT NULL,
  ref        bytea      NOT NULL,
  origin     int4,
  proposal   bytea      NOT NULL,
  expires_at timestamptz NOT NULL,
  PRIMARY KEY (group_id, epoch, ref)
);

-- index for lookups like `WHERE group_id = ? AND epoch = ? AND expires_at > now()`
CREATE INDEX mls_proposal_refs_group_epoch_expires_at_idx
  ON mls_proposal_refs (group_id, epoch, expires_at);

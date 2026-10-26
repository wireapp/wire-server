CREATE TABLE mls_proposal_refs (
  group_id bytea NOT NULL,
  epoch int8 NOT NULL,
  ref bytea NOT NULL,
  origin int4,
  proposal bytea NOT NULL,
  expires_at timestamptz NOT NULL,
  PRIMARY KEY (group_id, epoch, ref)
);

CREATE INDEX mls_proposal_refs_group_epoch_expires_idx
  ON mls_proposal_refs (group_id, epoch, expires_at);

CREATE INDEX mls_proposal_refs_expires_at_idx
  ON mls_proposal_refs (expires_at);

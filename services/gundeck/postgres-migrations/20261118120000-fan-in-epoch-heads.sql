-- Keep the latest epoch even when its membership is empty. This row also
-- provides the per-group lock for atomic membership replacement.
CREATE TABLE epoch_heads (
    group_id bytea PRIMARY KEY,
    epoch bigint NOT NULL
);

-- Epochs are Word64 values encoded as bigint, as in epoch_members. Negative
-- bigint values represent the upper half of Word64 and rank above nonnegative
-- values when selecting the latest epoch.
INSERT INTO epoch_heads (group_id, epoch)
SELECT DISTINCT ON (group_id) group_id, epoch FROM epoch_members
ORDER BY group_id, (epoch < 0) DESC, epoch DESC;

CREATE INDEX epoch_members_client ON epoch_members (user_id, client_id);
GRANT SELECT, INSERT, UPDATE, DELETE ON epoch_heads TO "wire-server";

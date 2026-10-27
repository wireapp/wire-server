-- WPB-29688: brig nonce (DPoP replay) store
--
-- No expires_at index / purge job, matching the password_reset precedent:
-- expiry is enforced on read and rows are consumed on use; unconsumed rows
-- are bounded by the 5-minute issuance TTL semantics and are tiny.

CREATE TABLE IF NOT EXISTS nonce (
  user_id uuid NOT NULL,
  key text NOT NULL,
  nonce uuid NOT NULL,
  expires_at timestamptz NOT NULL,
  PRIMARY KEY (user_id, key)
);

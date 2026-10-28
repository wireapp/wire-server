-- WPB-29688: brig nonce (DPoP replay) store

CREATE TABLE IF NOT EXISTS nonce (
  user_id uuid NOT NULL,
  key text NOT NULL,
  nonce uuid NOT NULL,
  expires_at timestamptz NOT NULL,
  PRIMARY KEY (user_id, key)
);

CREATE TABLE activation_keys (
  key text NOT NULL,
  key_type text NOT NULL,
  key_text text NOT NULL,
  code text NOT NULL,
  user_id uuid,
  retries int4 NOT NULL,
  expires_at timestamptz NOT NULL,
  PRIMARY KEY (key)
);


-- index for cleanup like `DELETE ... WHERE expires_at <= now()`
CREATE INDEX activation_keys_expires_at_idx
  ON activation_keys (expires_at);

CREATE TABLE conversation_codes (
  key text NOT NULL,
  scope integer NOT NULL,
  conversation uuid NOT NULL,
  password bytea,
  value text NOT NULL,
  expires_at timestamptz NOT NULL,
  PRIMARY KEY (key, scope)
);

-- index for lookups like `WHERE key = ? AND scope = ? AND expires_at > now()`
CREATE INDEX conversation_codes_key_scope_expires_at_idx
  ON conversation_codes (key, scope, expires_at);

-- index for deletes like `DELETE ... WHERE expires_at <= now()`
CREATE INDEX conversation_codes_expires_at_idx
  ON conversation_codes (expires_at);

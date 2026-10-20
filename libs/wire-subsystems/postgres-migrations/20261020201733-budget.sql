CREATE TABLE budget (
  key text NOT NULL,
  budget int4 NOT NULL,
  expires_at timestamptz NOT NULL,
  PRIMARY KEY (key)
);

-- Serves the (future) cleanup DELETE ... WHERE expires_at <= now(); lookups
-- are PK-only, and expired rows are filtered by expires_at > now().
CREATE INDEX budget_expires_at_idx ON budget (expires_at);

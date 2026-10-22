CREATE TABLE IF NOT EXISTS vcodes (
    key text NOT NULL,
    scope int4 NOT NULL,
    value text NOT NULL,
    retries int4 NOT NULL,
    email text NOT NULL,
    account uuid,
    expires_at timestamptz NOT NULL,
    PRIMARY KEY (key, scope)
);
CREATE INDEX vcodes_expires_at_idx ON vcodes (expires_at);
CREATE TABLE IF NOT EXISTS vcodes_throttle (
    key text NOT NULL,
    scope int4 NOT NULL,
    initial_delay int4 NOT NULL,
    expires_at timestamptz NOT NULL,
    PRIMARY KEY (key, scope)
);
CREATE INDEX vcodes_throttle_expires_at_idx ON vcodes_throttle (expires_at);

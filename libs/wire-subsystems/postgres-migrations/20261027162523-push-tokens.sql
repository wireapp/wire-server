-- Mirrors the Cassandra user_push table (cassandra-schema.cql); push tokens
-- have no TTL, so this is a copy-only migration with no expires/pg_cron.
CREATE TABLE user_push (
  usr uuid NOT NULL,
  ptoken text NOT NULL,
  app text NOT NULL,
  transport int4 NOT NULL,
  arn text,
  connection text NOT NULL,
  client text,
  PRIMARY KEY (usr, ptoken, app, transport)
);

-- Mirrors the Cassandra user_push table (cassandra-schema.cql); push tokens
-- have no TTL, so there is no expires_at column and no pg_cron job.
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

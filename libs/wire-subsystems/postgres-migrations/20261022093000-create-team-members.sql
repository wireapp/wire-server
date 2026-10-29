-- Team member tables (the member half of the Cassandra team store).
-- `perms` holds two smallints: the `self` and `copy` permission bitmasks
-- (mirroring the Cassandra `frozen<permissions>` UDT).  `invited_by` and
-- `invited_at` are both-or-neither (enforced at read time, as in Cassandra).
CREATE TABLE IF NOT EXISTS team_member (
  team uuid NOT NULL,
  "user" uuid NOT NULL,
  perms smallint[] NOT NULL,
  invited_by uuid,
  invited_at timestamptz,
  legalhold_status smallint,
  PRIMARY KEY (team, "user")
);

CREATE TABLE IF NOT EXISTS user_team (
  "user" uuid NOT NULL,
  team uuid NOT NULL,
  PRIMARY KEY ("user", team)
);

CREATE TABLE IF NOT EXISTS billing_team_member (
  team uuid NOT NULL,
  "user" uuid NOT NULL,
  PRIMARY KEY (team, "user")
);

CREATE TABLE IF NOT EXISTS team_admin (
  team uuid NOT NULL,
  "user" uuid NOT NULL,
  PRIMARY KEY (team, "user")
);

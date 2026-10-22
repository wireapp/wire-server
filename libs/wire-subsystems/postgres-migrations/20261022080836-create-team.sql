-- Team metadata (the `team` table of the Cassandra team store).
-- `created_at` is backfilled from Cassandra's writetime(binding) by the
-- team migration copy loop, so that GetTeamCreationTime keeps working after
-- the cutover.
CREATE TABLE IF NOT EXISTS team (
  team uuid PRIMARY KEY,
  creator uuid,
  name text,
  icon text,
  icon_key text,
  splash_screen text,
  binding boolean NOT NULL,
  status int NOT NULL,
  search_visibility int,
  deleted boolean NOT NULL DEFAULT false,
  created_at timestamptz
);

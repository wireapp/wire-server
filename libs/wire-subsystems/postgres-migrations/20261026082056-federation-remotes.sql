-- WPB-22964: migrate federation remotes (FederationConfigStore) from Cassandra.

CREATE TABLE IF NOT EXISTS federation_remotes (
  domain text PRIMARY KEY,
  search_policy integer NOT NULL,
  restriction integer
);

CREATE TABLE IF NOT EXISTS federation_remote_teams (
  domain text NOT NULL,
  team uuid NOT NULL,
  PRIMARY KEY (domain, team)
);

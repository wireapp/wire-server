CREATE TABLE IF NOT EXISTS federation_remote_teams (
  domain text NOT NULL,
  team uuid NOT NULL,
  PRIMARY KEY(domain, team)
);

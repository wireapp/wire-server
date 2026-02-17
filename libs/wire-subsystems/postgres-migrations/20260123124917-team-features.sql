CREATE TABLE team_features (
  team uuid NOT NULL,
  feature text NOT NULL,
  config jsonb,
  lock_status int,
  status int,
  PRIMARY KEY (team, feature)
);

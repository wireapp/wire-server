CREATE TABLE collaborators (
  user_id uuid,
  team_id uuid NOT NULL,
  permissions smallint[],
  PRIMARY KEY (user_id, team_id)
);

CREATE INDEX collaborators_team_id_idx ON collaborators (team_id);

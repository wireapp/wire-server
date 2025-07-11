-- TODO: Should the userId be unique? Probably.
-- TODO: Do we need an index on the team_id?
CREATE TABLE collaborators (
  user_id uuid,
  team_id uuid NOT NULL,
  permissions smallint[]
);

CREATE INDEX collaborators_team_id_idx ON collaborators (team_id);

CREATE TABLE apps (
  user_id uuid,
  team_id uuid NOT NULL,
  metadata json,
  PRIMARY KEY (user_id)
);

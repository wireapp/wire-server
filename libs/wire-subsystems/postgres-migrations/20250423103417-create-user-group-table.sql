CREATE TABLE user_group (
  team_id uuid NOT NULL,
  id uuid DEFAULT gen_random_uuid() UNIQUE,
  name text NOT NULL,
  managed_by int NOT NULL,
  PRIMARY KEY (team_id, id),
)

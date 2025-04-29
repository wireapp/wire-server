CREATE TABLE user_group (
  id uuid DEFAULT gen_random_uuid() PRIMARY KEY,
  team_id uuid NOT NULL,
  name text NOT NULL,
  managed_by int NOT NULL
)

CREATE TABLE user_group (
  id uuid DEFAULT gen_random_uuid() PRIMARY KEY,
  name text NOT NULL,
  managed_by int NOT NULL
)
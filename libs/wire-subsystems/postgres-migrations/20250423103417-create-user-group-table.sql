CREATE TABLE user_group (
  team_id uuid NOT NULL,
  id uuid DEFAULT gen_random_uuid() UNIQUE,
  name text NOT NULL,
  managed_by int NOT NULL,
  created_at timestamptz DEFAULT now(),
  PRIMARY KEY (team_id, id),
  -- (0 :: int) is sometimes represented as four NULL bytes by
  -- postgres, which can cause parser problems in hasql:
  CONSTRAINT managed_by_nonzero
)

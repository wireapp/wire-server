CREATE TYPE permission_flag AS ENUM ('create_team_conversation', 'implicit_connection');

CREATE TABLE collaborators (
  user_id uuid,
  team_id uuid NOT NULL,
  permissions permission_flag []
);

-- Client index (galley): user-client index replacing the Cassandra
-- clients.clients set column.
CREATE TABLE IF NOT EXISTS user_client_index (
  user_id uuid NOT NULL,
  client_id text NOT NULL,
  PRIMARY KEY (user_id, client_id)
);

CREATE TABLE mls_history_client (
  group_id bytea NOT NULL,
  id uuid NOT NULL,
  leaf_node_index integer NOT NULL,
  removal_pending boolean NOT NULL,
  PRIMARY KEY (group_id, id)
);

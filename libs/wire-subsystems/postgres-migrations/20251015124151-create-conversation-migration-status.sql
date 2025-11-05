CREATE TABLE conversation_migration_pending_deletes (
  typ text NOT NULL,
  id uuid NOT NULL,
  PRIMARY KEY (typ, id)
  );

CREATE TABLE conversation_out_of_sync (
  conv_id uuid PRIMARY KEY REFERENCES conversation (id) ON DELETE CASCADE,
  out_of_sync boolean NOT NULL
);

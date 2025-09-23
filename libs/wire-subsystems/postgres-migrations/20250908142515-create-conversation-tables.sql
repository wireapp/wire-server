CREATE TABLE conversation (
  id uuid PRIMARY KEY,
  access integer[],
  access_roles_v2 integer[],
  cells_state integer,
  channel_add_permission integer,
  cipher_suite integer,
  creator uuid,
  epoch bigint,
  epoch_timestamp timestamptz,
  group_conv_type integer,
  group_id bytea,
  message_timer bigint,
  name text,
  protocol integer,
  public_group_state bytea,
  receipt_mode integer,
  team uuid,
  type integer NOT NULL,
  parent_conv uuid REFERENCES conversation (id) ON DELETE CASCADE
);

CREATE INDEX conversation_team_idx ON conversation (team);

CREATE TABLE subconversation (
  conv_id uuid NOT NULL REFERENCES conversation (id) ON DELETE CASCADE,
  subconv_id text NOT NULL,
  cipher_suite integer,
  epoch bigint,
  epoch_timestamp timestamptz,
  group_id bytea,
  public_group_state bytea,
  PRIMARY KEY (conv_id, subconv_id)
);

CREATE TABLE conversation_member (
  conv uuid NOT NULL REFERENCES conversation (id) ON DELETE CASCADE,
  "user" uuid NOT NULL,
  conversation_role text,
  hidden boolean,
  hidden_ref text,
  otr_archived boolean,
  otr_archived_ref text,
  otr_muted boolean,
  otr_muted_ref text,
  otr_muted_status integer,
  provider uuid,
  service uuid,
  status integer,
  PRIMARY KEY (conv, "user")
);

CREATE INDEX conversation_member_user_idx ON conversation_member (user)

CREATE TABLE remote_conversation_local_member (
  "user" uuid NOT NULL,
  conv_remote_domain text NOT NULL,
  conv_remote_id uuid NOT NULL,
  hidden boolean,
  hidden_ref text,
  otr_archived boolean,
  otr_archived_ref text,
  otr_muted_ref text,
  otr_muted_status integer,
  PRIMARY KEY ("user", conv_remote_domain, conv_remote_id)
);

CREATE TABLE local_conversation_remote_member (
  conv uuid NOT NULL REFERENCES conversation (id) ON DELETE CASCADE,
  user_remote_domain text NOT NULL,
  user_remote_id uuid NOT NULL,
  conversation_role text,
  PRIMARY KEY (conv, user_remote_domain, user_remote_id)
);

CREATE TABLE mls_group_member_client (
  group_id bytea NOT NULL,
  user_domain text NOT NULL,
  "user" uuid NOT NULL,
  client text NOT NULL,
  key_package_ref bytea,
  leaf_node_index integer NOT NULL,
  removal_pending boolean NOT NULL,
  PRIMARY KEY (group_id, user_domain, user, client)
);

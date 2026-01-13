CREATE TABLE wire_user (
  id uuid PRIMARY KEY,
  user_type integer NOT NULL,
  accent_id integer NOT NULL,
  activated boolean NOT NULL,
  country text,
  email text,
  email_unvalidated text,
  expires timestamptz,
  feature_conference_calling integer,
  handle text UNIQUE,
  language text,
  managed_by integer,
  name text NOT NULL,
  password text,
  picture jsonb,
  provider uuid,
  service uuid,
  searchable boolean,
  sso_id jsonb,
  account_status integer,
  supported_protocols integer,
  team uuid,
  text_status text,
  rich_info jsonb,
  created_at timestamptz NOT NULL DEFAULT current_timestamp,
  updated_at timestamptz NOT NULL DEFAULT current_timestamp
);

CREATE INDEX wire_user_service_idx ON wire_user(provider, service);

CREATE OR REPLACE FUNCTION update_updated_at()
  RETURNS TRIGGER AS $$
BEGIN
  NEW.updated_at = now();
  RETURN NEW;
END;
$$ language 'plpgsql';

CREATE TRIGGER update_user_updated_at BEFORE UPDATE ON wire_user FOR EACH ROW EXECUTE PROCEDURE update_updated_at();

CREATE TABLE asset (
  user_id uuid NOT NULL,
  typ integer NOT NULL,
  key text NOT NULL,
  size integer
);

CREATE INDEX asset_user_id_idx ON asset (user_id);

CREATE TABLE bot_conv (
  id uuid PRIMARY KEY,
  conv uuid NOT NULL,
  conv_team uuid,
  FOREIGN KEY (id) REFERENCES wire_user(id) ON DELETE CASCADE
);

CREATE INDEX bot_conv_conv_idx ON bot_conv (conv);
CREATE INDEX bot_conv_team_idx ON bot_conv (conv_team);

CREATE TABLE deleted_user (
  id uuid PRIMARY KEY
);

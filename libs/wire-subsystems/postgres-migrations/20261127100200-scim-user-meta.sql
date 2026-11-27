CREATE TABLE IF NOT EXISTS scim_user_times (
  uid uuid PRIMARY KEY,
  created_at timestamptz NOT NULL,
  last_updated_at timestamptz NOT NULL,
  email_type text,
  email_primary boolean
);

create table profile_links (
  user_id uuid NOT NULL,
  link_name text NOT NULL,
  url text NOT NULL,
  verified_at timestamptz,
  PRIMARY KEY (user_id, link_name)
  );

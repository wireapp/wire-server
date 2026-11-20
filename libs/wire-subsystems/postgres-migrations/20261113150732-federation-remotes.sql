CREATE TABLE IF NOT EXISTS federation_remotes (
  domain text PRIMARY KEY,
  search_policy integer NOT NULL,
  restriction integer
);
CREATE TABLE channel_user_group (
  channel_id uuid,
  user_group_id uuid,
  PRIMARY KEY (channel_id),
  CONSTRAINT fk_user_group
    FOREIGN KEY (user_group_id)
      REFERENCES user_group(id)
)

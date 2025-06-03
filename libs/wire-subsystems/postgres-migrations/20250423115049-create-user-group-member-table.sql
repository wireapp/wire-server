CREATE TABLE user_group_member (
  user_group_id uuid,
  user_id uuid,
  PRIMARY KEY (user_group_id, user_id),
  CONSTRAINT fk_user_group
    FOREIGN KEY (user_group_id)
      REFERENCES user_group(id)
      ON DELETE CASCADE
)

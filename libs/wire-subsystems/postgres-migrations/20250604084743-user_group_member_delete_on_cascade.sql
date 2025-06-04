ALTER TABLE user_group_member
DROP CONSTRAINT fk_user_group,
ADD CONSTRAINT fk_user_group
    FOREIGN KEY (user_group_id)
    REFERENCES user_group(id)
    ON DELETE CASCADE;

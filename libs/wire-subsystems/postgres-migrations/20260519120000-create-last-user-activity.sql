CREATE TABLE last_user_activity (
    user_id   uuid        PRIMARY KEY,
    active_at timestamptz NOT NULL
);

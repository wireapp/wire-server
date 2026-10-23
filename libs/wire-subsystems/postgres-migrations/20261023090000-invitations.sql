CREATE TABLE IF NOT EXISTS team_invitation (
    team uuid NOT NULL,
    invitation_id uuid NOT NULL,
    role int4,
    created_at timestamptz NOT NULL,
    created_by uuid,
    email text NOT NULL,
    name text,
    code text NOT NULL,
    expires_at timestamptz NOT NULL,
    PRIMARY KEY (team, invitation_id)
);
CREATE INDEX IF NOT EXISTS team_invitation_code_idx ON team_invitation (code);
CREATE INDEX IF NOT EXISTS team_invitation_email_idx ON team_invitation (email);
CREATE TABLE IF NOT EXISTS team_scim_pending_user_email (
    team uuid NOT NULL,
    email text NOT NULL,
    user_id uuid NOT NULL,
    PRIMARY KEY (team, email, user_id)
);

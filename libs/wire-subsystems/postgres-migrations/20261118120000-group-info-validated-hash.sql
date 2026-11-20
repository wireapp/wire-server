-- WPB-29949: SHA-256 digest of the stored group info, recorded only when its
-- ratchet tree matched the membership the server validated for the commit.
-- The ghost-client repair only trusts a group info whose digest matches.
ALTER TABLE conversation ADD COLUMN IF NOT EXISTS group_info_validated_hash bytea;
ALTER TABLE subconversation ADD COLUMN IF NOT EXISTS group_info_validated_hash bytea;

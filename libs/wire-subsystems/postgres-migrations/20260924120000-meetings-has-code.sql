-- WPB-28987: meetings created before the join-link feature have no row in
-- conversation_codes; has_code = false makes read paths serve the
-- null-uuid placeholder link.
ALTER TABLE meetings ADD COLUMN has_code BOOLEAN NOT NULL DEFAULT FALSE;

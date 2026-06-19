-- Distinguish conversation codes from meeting codes. Existing rows refer to
-- conversations, so the default is 'conv'. Meeting codes store 'meeting'.
ALTER TABLE conversation_codes
  ADD COLUMN target text DEFAULT 'conv' NOT NULL;

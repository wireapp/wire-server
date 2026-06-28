-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

-- Migration: Add encrypted conversation description table
-- Description: Stores one encrypted description blob per conversation or channel

CREATE TABLE IF NOT EXISTS conversation_description (
    conv_id uuid PRIMARY KEY REFERENCES conversation (id) ON DELETE CASCADE,
    version bigint NOT NULL,
    ciphertext bytea NOT NULL,
    created_at timestamptz NOT NULL DEFAULT current_timestamp,
    updated_at timestamptz NOT NULL DEFAULT current_timestamp
);

CREATE TRIGGER update_conversation_description_updated_at
BEFORE UPDATE ON conversation_description
FOR EACH ROW
EXECUTE PROCEDURE update_updated_at();

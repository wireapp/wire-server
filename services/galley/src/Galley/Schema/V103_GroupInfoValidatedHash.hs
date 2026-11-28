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
module Galley.Schema.V103_GroupInfoValidatedHash
  ( migration,
  )
where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

-- | SHA-256 digest of the stored group info, recorded only when its ratchet
-- tree matched the membership the server validated for the commit (WPB-29949).
migration :: Migration
migration =
  Migration 103 "add column group_info_validated_hash to conversation and subconversation" $ do
    schema'
      [r| ALTER TABLE conversation ADD (
            group_info_validated_hash blob
        )
     |]
    schema'
      [r| ALTER TABLE subconversation ADD (
            group_info_validated_hash blob
        )
     |]

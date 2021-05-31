-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module V50_AddLegalholdWhitelisted
  ( migration,
  )
where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

-- This migration replaces the remote identifiers deleted in migration 48 with separate tables.
-- This change occurs because we decided to stop using opaque Ids. Instead, we'll be explict with remote identifiers.
-- Since two backends may have a conversation or user with the same UUID
-- (whether by chance or maliciously so), this change guarantees we don't
-- accidentally override information about a conversation on one backend by
-- information about a conversation on another backend.
migration :: Migration
migration = Migration 50 "Add table that defines whitelisted teams if for the FeatureLegalHoldWhitelistTeamsAndImplicitConsent feature." $ do
  -- The user_remote_conv (similar to the user) table answers the question:
  -- Which conversations am I a member of?
  -- With federation one now also needs to know: Where are these conversations located?
  -- This table stores *local* users who are part of *remote* conversations
  schema'
    [r|
      CREATE TABLE legalhold_whitelisted (
        team uuid,
        PRIMARY KEY (team)
      )
    |]

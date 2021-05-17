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

module V49_ReAddRemoteIdentifiers
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
migration = Migration 49 "Add tables for remote users in local conversations or remote conversations in local membership" $ do
  -- The user_remote_conv (similar to the user) table answers the question:
  -- Which conversations am I a member of?
  -- With federation one now also needs to know: Where are these conversations located?
  -- This table stores *local* users who are part of *remote* conversations
  schema'
    [r|
      CREATE TABLE user_remote_conv (
        user uuid,
        conv_remote_domain text,
        conv_remote_id uuid,
        PRIMARY KEY (user, conv_remote_domain, conv_remote_id)
      ) WITH compaction = {'class': 'LeveledCompactionStrategy'};
    |]

  -- The member_remote (similar to the member) table answers the question:
  -- Which users are part of a conversation?
  -- With federation one now also needs to know: Where are these users located?
  -- This table stores *remote* users who are part of *local* conversations
  schema'
    [r|
      CREATE TABLE member_remote_user (
        conv uuid,
        user_remote_domain text,
        user_remote_id uuid,
        conversation_role text,
        PRIMARY KEY (conv, user_remote_domain, user_remote_id)
      ) WITH compaction = {'class': 'LeveledCompactionStrategy'};
    |]

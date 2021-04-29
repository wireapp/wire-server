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

module V48_ChangeRemoteIdentifiers
  ( migration,
  )
where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

-- This migration deletes the (so far unused) entries introduced in migration 44 and replaces
-- them with a separate table.
migration :: Migration
migration = Migration 48 "Change schema for remote identifiers to conversation related tables" $ do
  -- The user table answers the question: Which conversations am I a member of?
  -- With federation one now also needs to know: Where are these conversations located?
  schema'
    [r|
      ALTER TABLE user DROP (
        conv_remote_id,
        conv_remote_domain
      );
    |]
  -- The member table is used to answer the question: Which users are part of a conversation?
  -- With federation one now also needs to know: Where are these users located?
  schema'
    [r|
      ALTER TABLE member DROP (
        user_remote_id,
        user_remote_domain
      );
    |]

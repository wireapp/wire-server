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

-- This migration deletes the (so far unused) entries introduced in migration 44
-- and replaces them with separate tables. This change occurs because we
-- decided to stop using opaque Ids and to be explict with remote identifiers.
-- However, due to the way the current user and member table primary key setup,
-- we cannot keep using those tables without specifying a local user or
-- conversation id.
-- Since two backends may have a conversation or user with the same UUID
-- (whether by chance or maliciously so), this change guarantees we don't
-- accidentally override information about a conversation on one backend by
-- information about a conversation on another backend.
migration :: Migration
migration = Migration 48 "Change schema for remote identifiers to conversation related tables" $ do
  -- Remove unused columns introduced in migration 44
  schema'
    [r|
      ALTER TABLE user DROP (
        conv_remote_id,
        conv_remote_domain
      );
    |]
  schema'
    [r|
      ALTER TABLE member DROP (
        user_remote_id,
        user_remote_domain
      );
    |]

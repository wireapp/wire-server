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

module V48_DeleteRemoteIdentifiers
  ( migration,
  )
where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

-- This migration deletes the (so far unused) entries introduced in migration 44
-- as we decided to stop using opaque Ids and to be explict with remote identifiers.
migration :: Migration
migration = Migration 48 "Delete remote identifiers introduced in migration 44" $ do
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

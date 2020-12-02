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

module V62_users_pending_activation (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration =
  Migration 62 "Add users_pending_activation" $
    -- | This table keeps track of users that were invited via SCIM.
    --   When their invitation expires this table is used
    --   to clean any data of these expired users.

    -- The column expires_at_day is the date of expiry.
    -- It is encoded as 'int' because cql-io doesn't seem to work with 'date' types.
    schema'
      [r|
        CREATE TABLE users_pending_activation
        (
          expires_at_day  int
        , user            uuid
        , team            uuid
        , primary key (expires_at_day, user)
        )
        with clustering order by (user ASC)
      |]

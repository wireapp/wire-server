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
    -- | Column expires_at_day is the date of column expires_at
    -- We use int for the encoding instead of date,
    -- because the cql-io lib doesn't seem to implement date literals
    -- TODO(stefan) try 'date'
    schema'
      [r|
        CREATE TABLE users_pending_activation
        (
          expires_at_day  date
        , user            uuid
        , team            uuid
        , primary key (expires_at_day, user)
        )
        with clustering order by (user ASC)
      |]

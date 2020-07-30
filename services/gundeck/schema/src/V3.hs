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

module V3
  ( migration,
  )
where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 3 "Add clients table, push.client and user_push.client" $ do
  schema'
    [r|
        create columnfamily if not exists clients
            ( user    uuid -- user id
            , client  text -- client id
            , enckey  blob -- native push encryption key
            , mackey  blob -- native push mac key
            , primary key (user, client)
            ) with compaction = { 'class' : 'LeveledCompactionStrategy' };
        |]
  schema' [r| alter columnfamily user_push add client text; |]
  schema' [r| alter columnfamily push add client text; |]

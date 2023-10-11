-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
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

module Gundeck.Schema.V10
  ( migration,
  )
where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 10 "Add notification payload references" $ do
  schema' [r| ALTER TABLE notifications ADD payload_ref uuid; |]

  schema' [r| ALTER TABLE notifications ADD payload_ref_size int; |]

  schema'
    [r|
        CREATE COLUMNFAMILY IF NOT EXISTS notification_payload
            ( id       uuid PRIMARY KEY -- payload id
            , payload blob
            ) with compaction = { 'class' : 'LeveledCompactionStrategy' };
        |]

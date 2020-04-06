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

module V7
  ( migration,
  )
where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration =
  Migration 7 "Add notifications column family" $
    schema'
      [r|
        create columnfamily if not exists notifications
            ( user    uuid      -- user id
            , id      timeuuid  -- notification id
            , payload blob      -- notification payload
            , clients set<text> -- intended recipients (empty=all)
            , primary key (user, id)
            ) with clustering order by (id asc)
               and compaction  = { 'class'               : 'LeveledCompactionStrategy'
                                 , 'tombstone_threshold' : 0.1 }
               and compression = { 'sstable_compression' : 'LZ4Compressor' }
               and gc_grace_seconds = 0;
        |]

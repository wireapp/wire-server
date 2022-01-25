-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module V45_AddFederationIdMapping
  ( migration,
  )
where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

-- | See <https://github.com/wearezeta/documentation/blob/master/topics/federation/federation-design.md#namespaces-and-user-identity>.
migration :: Migration
migration = Migration 45 "Add ID mapping for federation" $ do
  schema'
    [r|
        CREATE TABLE id_mapping (
            mapped_id uuid PRIMARY KEY,
            remote_id uuid,
            remote_domain text,
        ) WITH compaction = {'class': 'org.apache.cassandra.db.compaction.LeveledCompactionStrategy'}
           AND gc_grace_seconds = 864000;
    |]

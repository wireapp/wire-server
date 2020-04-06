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

module V20 (migration) where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 20 "Initial schema" $ do
  schema'
    [r|
        CREATE TABLE conversation (
            conv    uuid PRIMARY KEY,
            access  set<int>,
            creator uuid,
            name    text,
            type    int
        ) WITH compaction = {'class': 'org.apache.cassandra.db.compaction.LeveledCompactionStrategy'}
            AND gc_grace_seconds = 864000;
        |]
  schema'
    [r|
        CREATE TABLE member (
            conv             uuid,
            user             uuid,
            status           int,
            hidden           boolean,
            hidden_ref       text,
            otr_archived     boolean,
            otr_archived_ref text,
            otr_muted        boolean,
            otr_muted_ref    text,
            provider         uuid,
            service          uuid,
            PRIMARY KEY (conv, user)
        ) WITH CLUSTERING ORDER BY (user ASC)
            AND compaction = {'class': 'org.apache.cassandra.db.compaction.LeveledCompactionStrategy'}
            AND gc_grace_seconds = 864000;
        |]
  schema'
    [r|
        CREATE TABLE user (
            user uuid,
            conv uuid,
            PRIMARY KEY (user, conv)
        ) WITH CLUSTERING ORDER BY (conv ASC)
            AND compaction = {'class': 'org.apache.cassandra.db.compaction.LeveledCompactionStrategy'}
            AND gc_grace_seconds = 864000;
        |]
  schema'
    [r|
        CREATE TABLE clients (
            user uuid PRIMARY KEY,
            clients set<text>
        ) WITH compaction = {'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy'}
            AND gc_grace_seconds = 864000;
        |]
  schema'
    [r|
        CREATE TABLE service (
            provider     uuid,
            id           uuid,
            auth_token   ascii,
            base_url     blob,
            enabled      boolean,
            fingerprints set<blob>,
            PRIMARY KEY (provider, id)
        ) WITH CLUSTERING ORDER BY (id ASC)
            AND compaction = {'class': 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy'}
            AND gc_grace_seconds = 864000;
        |]

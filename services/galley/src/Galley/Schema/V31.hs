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

module Galley.Schema.V31
  ( migration,
  )
where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 31 "Add legalhold tables" $ do
  schema'
    [r|
        CREATE TABLE legalhold_service (
            team_id      uuid,
            base_url     blob,
            fingerprint  blob,
            auth_token   ascii,
            PRIMARY KEY (team_id)
        ) WITH compaction = {'class': 'LeveledCompactionStrategy'}
          AND gc_grace_seconds = 864000;
    |]
  schema'
    [r|
        CREATE TABLE legalhold_team_config (
            team_id      uuid,
            status       int,
            PRIMARY KEY (team_id)
        ) WITH compaction = {'class': 'LeveledCompactionStrategy'}
          AND gc_grace_seconds = 864000;
    |]
  schema'
    [r|
        CREATE TABLE legalhold_pending_prekeys (
            user      uuid,
            key       int,
            data      text,
            PRIMARY KEY (user, key)
        ) WITH compaction = {'class': 'LeveledCompactionStrategy'}
          AND gc_grace_seconds = 864000;
    |]
  schema'
    [r|
        CREATE TABLE legalhold_user_status (
                user      uuid,
                status    int,
                PRIMARY KEY (user)
        ) WITH compaction = {'class': 'LeveledCompactionStrategy'}
          AND gc_grace_seconds = 864000;
    |]

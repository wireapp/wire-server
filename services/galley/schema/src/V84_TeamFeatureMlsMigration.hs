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

module V84_TeamFeatureMlsMigration
  ( migration,
  )
where

import Cassandra.Schema
import Imports
import Text.RawString.QQ

migration :: Migration
migration = Migration 84 "Add feature config for team feature MLS Migration" $ do
  schema'
    [r| ALTER TABLE team_features ADD (
          mls_migration_status int,
          mls_migration_lock_status int,
          mls_migration_start_time timestamp,
          mls_migration_finalise_regardless_after timestamp,
          mls_migration_users_threshold int,
          mls_migration_clients_threshold int
        )
     |]

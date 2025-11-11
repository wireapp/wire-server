{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.IndexedUserStore.Bulk where

import Polysemy
import Wire.UserSearch.Migration

-- | Increase this number any time you want to force reindexing.
expectedMigrationVersion :: MigrationVersion
expectedMigrationVersion = MigrationVersion 6

-- | Bulk operations, must not be used from any web handler
data IndexedUserStoreBulk m a where
  -- | Only changes data if it is not updated since last update, use when users
  -- need to be synced because of an outage, or migrating to a new ES instance.
  SyncAllUsers :: IndexedUserStoreBulk m ()
  -- | Overwrite all users in the ES index, use it when trying to fix some
  -- inconsistency or while introducing a new field in the mapping.
  ForceSyncAllUsers :: IndexedUserStoreBulk m ()
  MigrateData :: IndexedUserStoreBulk m ()

makeSem ''IndexedUserStoreBulk

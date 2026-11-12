{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

-- | Metadata of automatic backup files. Blobs live elsewhere; deletes return the removed ids so
-- callers can delete the blobs.
module Wire.BackupStore where

import Data.Id (ClientId, UserId)
import Imports
import Polysemy
import Wire.API.Backup
import Wire.BackupSubsystem.Rules

data NewBackupFile = NewBackupFile
  { id :: BackupFileId,
    user :: UserId,
    client :: ClientId,
    epoch :: BackupEpoch,
    size :: Word64
  }
  deriving stock (Eq, Show)

data BackupStore m a where
  -- | Atomically re-reads the device state, applies 'checkUpload' and inserts.
  InsertBackupFile ::
    BackupLimits ->
    NewBackupFile ->
    BackupStore m (Either UploadRejection BackupFile)
  -- | File count per stored epoch of the device.
  GetDeviceEpochCounts :: UserId -> ClientId -> BackupStore m (Map BackupEpoch Word32)
  -- | Ordered by creation time.
  ListBackupFiles :: UserId -> ClientId -> BackupEpoch -> BackupStore m [BackupFile]
  LookupBackupFile ::
    UserId ->
    ClientId ->
    BackupEpoch ->
    BackupFileId ->
    BackupStore m (Maybe BackupFile)
  DeleteBackupFileRow :: BackupFileId -> BackupStore m ()
  DeleteEpochRows :: UserId -> ClientId -> BackupEpoch -> BackupStore m [BackupFileId]
  DeleteClientRows :: UserId -> ClientId -> BackupStore m [BackupFileId]
  DeleteUserRows :: UserId -> BackupStore m [BackupFileId]

makeSem ''BackupStore

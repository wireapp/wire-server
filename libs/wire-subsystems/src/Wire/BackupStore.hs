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
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Wire.BackupStore where

import Data.Id
import Imports
import Polysemy
import Wire.API.Backup

data CommitFullError
  = -- | The epoch is not accepted (anymore) under the lock.
    CommitEpochInvalid
  | -- | The reservation was already removed, e.g. by the cleanup.
    CommitReservationGone
  deriving (Eq, Show)

data BackupStore m a where
  -- | Insert an upload reservation (an orphan with @file_created_at NULL@).
  ReserveUpload :: UserId -> ClientId -> BackupEpoch -> BackupFileId -> BackupStore m ()
  -- | One transaction under the per-client lock: re-check the epoch rule, claim the
  -- reservation, add the full to the epoch (orphaning a replaced full) and orphan the
  -- fulls of stale epochs.
  CommitFull ::
    UserId ->
    ClientId ->
    BackupEpoch ->
    BackupFileId ->
    Word64 ->
    BackupStore m (Either CommitFullError BackupFile)
  LookupMaxEpoch :: UserId -> ClientId -> BackupStore m (Maybe BackupEpoch)
  LookupEpochFiles :: UserId -> ClientId -> BackupEpoch -> BackupStore m (Maybe BackupFiles)
  -- | Is the file the full of the epoch?
  LookupFull :: UserId -> ClientId -> BackupEpoch -> BackupFileId -> BackupStore m Bool

makeSem ''BackupStore

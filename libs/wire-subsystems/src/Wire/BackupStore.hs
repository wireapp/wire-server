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

data CommitIncrementalError
  = -- | The epoch does not exist or its current full is another one.
    CommitFullMissing
  | -- | The epoch already holds the maximal number of files.
    CommitEpochFull
  | -- | The reservation was already removed, e.g. by the cleanup.
    CommitIncrementalReservationGone
  deriving (Eq, Show)

data BackupStore m a where
  -- | Insert an upload reservation (an orphan with @file_created_at NULL@).
  ReserveUpload :: UserId -> ClientId -> BackupEpoch -> BackupFileId -> BackupStore m ()
  -- | One transaction under the per-client lock: re-check the epoch rule, claim the
  -- reservation, add the full to the epoch (orphaning a replaced full and its
  -- incrementals) and orphan the fulls and incrementals of stale epochs.
  CommitFull ::
    UserId ->
    ClientId ->
    BackupEpoch ->
    BackupFileId ->
    Word64 ->
    BackupStore m (Either CommitFullError BackupFile)
  -- | One transaction under the per-client lock: re-check that the full is the epoch's
  -- current one and that the epoch has room for another file (the first argument is
  -- @maxFilesPerEpoch@, counting the full), claim the reservation and add the incremental.
  -- Arguments: limit, user, client, epoch, full, new file, size.
  CommitIncremental ::
    Word32 ->
    UserId ->
    ClientId ->
    BackupEpoch ->
    BackupFileId ->
    BackupFileId ->
    Word64 ->
    BackupStore m (Either CommitIncrementalError BackupFile)
  LookupMaxEpoch :: UserId -> ClientId -> BackupStore m (Maybe BackupEpoch)
  LookupEpochFiles :: UserId -> ClientId -> BackupEpoch -> BackupStore m (Maybe BackupFiles)
  -- | Is the file the full of the epoch?
  LookupFull :: UserId -> ClientId -> BackupEpoch -> BackupFileId -> BackupStore m Bool
  -- | Is the incremental (last argument) one of the full (second to last) of the epoch?
  LookupIncremental ::
    UserId -> ClientId -> BackupEpoch -> BackupFileId -> BackupFileId -> BackupStore m Bool
  -- | Number of current files of the epoch including the full; 'Nothing' if there is no epoch.
  CountEpochFiles :: UserId -> ClientId -> BackupEpoch -> BackupStore m (Maybe Word32)
  -- | Summaries of the current files of the epochs of the user, of one client or of all
  -- (no lock, read only). Ascending by client and epoch; clients without epochs are absent.
  ListClientBackups :: UserId -> Maybe ClientId -> BackupStore m [ClientBackups]

makeSem ''BackupStore

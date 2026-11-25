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
import Data.Time.Clock (NominalDiffTime)
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
  -- | Insert an upload reservation (an orphan with @file_created_at NULL@). A commit claims
  -- it only while @deletion_last_tried@ is NULL.
  ReserveUpload :: UserId -> ClientId -> BackupEpoch -> BackupFileId -> BackupStore m ()
  -- | One transaction under the per-client lock: re-check the epoch rule, claim the
  -- reservation, add the full to the epoch (orphaning a replaced full and its
  -- incrementals) and orphan the fulls and incrementals of stale epochs. The list holds
  -- the ids orphaned this way.
  CommitFull ::
    UserId ->
    ClientId ->
    BackupEpoch ->
    BackupFileId ->
    Word64 ->
    BackupStore m (Either CommitFullError (BackupFile, [BackupFileId]))
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
  -- | Number of current files of the epoch including the full; 'Nothing' if there is no epoch
  -- or the file (last argument) is not its full.
  CountEpochFiles ::
    UserId -> ClientId -> BackupEpoch -> BackupFileId -> BackupStore m (Maybe Word32)
  -- | Summaries of the current files of the epochs of the user, of one client or of all
  -- (no lock, read only). Ascending by client and epoch; clients without epochs are absent.
  ListClientBackups :: UserId -> Maybe ClientId -> BackupStore m [ClientBackups]
  -- | The clients of the user that have epochs or in-flight upload reservations (no lock,
  -- read only).
  ListBackupClients :: UserId -> BackupStore m [ClientId]
  -- | One transaction under the per-client lock: orphan all committed files (incrementals
  -- and fulls) of the client, keeping their creation times. Upload reservations stay but are
  -- marked (@deletion_last_tried@), so committing them fails with the reservation gone.
  -- Returns the orphaned ids.
  OrphanClient :: UserId -> ClientId -> BackupStore m [BackupFileId]
  -- | One transaction: pick up to the given number of orphans to delete (deletions of
  -- committed files, and upload reservations older than the grace period; never-tried ones
  -- first) and mark them as tried. Arguments: grace period, batch size. Concurrent calls may
  -- return the same ids, deleting is idempotent.
  SelectCleanupBatch :: NominalDiffTime -> Word32 -> BackupStore m [BackupFileId]
  -- | Remove orphan rows by id (no lock).
  DeleteOrphanRows :: [BackupFileId] -> BackupStore m ()
  -- | Set @deletion_last_tried@ of orphan rows by id (no lock).
  MarkDeletionTried :: [BackupFileId] -> BackupStore m ()

makeSem ''BackupStore

-- | Orphans the backups of all clients of the user, one transaction per client. Idempotent
-- and resumable: a retry continues with the clients that still have epochs or reservations.
-- Callers run it again after the clients are removed: an epoch-1 upload racing the removal
-- can commit after the first run.
orphanUserBackups :: (Member BackupStore r) => UserId -> Sem r ()
orphanUserBackups uid = listBackupClients uid >>= traverse_ (orphanClient uid)

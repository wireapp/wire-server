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

module Wire.MockInterpreters.BackupStore where

import Data.Id
import Data.Json.Util (fromUTCTimeMillis, toUTCTimeMillis)
import Data.Map qualified as Map
import Data.Time.Clock (UTCTime)
import Imports
import Polysemy
import Polysemy.State
import Wire.API.Backup
import Wire.BackupStore
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now

data BackupState = BackupState
  { -- | Epoch creation time and full of each epoch.
    epochs :: Map (UserId, ClientId, BackupEpoch) (UTCTime, BackupFile),
    -- | Orphans with their @file_created_at@ ('Nothing' for upload reservations).
    orphans :: Map BackupFileId (Maybe UTCTime)
  }
  deriving (Eq, Show)

emptyBackupState :: BackupState
emptyBackupState = BackupState {epochs = mempty, orphans = mempty}

-- | Mirrors the transaction of "Wire.BackupStore.Postgres".
inMemoryBackupStore ::
  (Member (State BackupState) r, Member Now r) =>
  InterpreterFor BackupStore r
inMemoryBackupStore = interpret $ \case
  ReserveUpload _ _ _ fid ->
    modify $ \s -> s {orphans = Map.insert fid Nothing s.orphans}
  CommitFull uid cid epoch fid size -> do
    s <- get
    now <- Now.get
    let maxEpoch = maxEpochOf s uid cid
        file = BackupFile {id = fid, size = size, createdAt = toUTCTimeMillis now}
    case () of
      _
        | not (epochAccepted maxEpoch epoch) -> pure (Left CommitEpochInvalid)
        | Map.lookup fid s.orphans /= Just Nothing -> pure (Left CommitReservationGone)
        | otherwise -> do
            let key = (uid, cid, epoch)
                withoutReservation = Map.delete fid s.orphans
                replaced = case Map.lookup key s.epochs of
                  Nothing -> mempty
                  Just (_, old) -> Map.singleton old.id (Just (fromUTCTimeMillis old.createdAt))
                inserted = Map.insert key (now, file) s.epochs
                isStale (u, c, e) = u == uid && c == cid && epoch >= BackupEpoch 3 && e <= staleEpochs epoch
                (stale, kept) = Map.partitionWithKey (\k _ -> isStale k) inserted
                staleOrphans =
                  Map.fromList [(f.id, Just (fromUTCTimeMillis f.createdAt)) | (_, f) <- Map.elems stale]
            put BackupState {epochs = kept, orphans = withoutReservation <> replaced <> staleOrphans}
            pure (Right file)
  LookupMaxEpoch uid cid -> gets (\s -> maxEpochOf s uid cid)
  LookupEpochFiles uid cid epoch ->
    gets $ \s ->
      (\(_, f) -> BackupFiles {full = f, incrementals = []}) <$> Map.lookup (uid, cid, epoch) s.epochs

maxEpochOf :: BackupState -> UserId -> ClientId -> Maybe BackupEpoch
maxEpochOf s uid cid =
  fmap fst . Map.lookupMax . Map.fromList $
    [(e, ()) | (u, c, e) <- Map.keys s.epochs, u == uid, c == cid]

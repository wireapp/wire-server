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

module Wire.MockInterpreters.BackupStore
  ( BackupStoreState,
    inMemoryBackupStoreInterpreter,
  )
where

import Data.Id
import Data.Json.Util (toUTCTimeMillis)
import Data.Map qualified as Map
import Imports
import Polysemy
import Polysemy.State
import Wire.API.Backup
import Wire.BackupStore
import Wire.BackupSubsystem.Rules
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now

type BackupStoreState = Map BackupFileId (UserId, ClientId, BackupEpoch, BackupFile)

inMemoryBackupStoreInterpreter ::
  (Member (State BackupStoreState) r, Member Now r) =>
  InterpreterFor BackupStore r
inMemoryBackupStoreInterpreter = interpret $ \case
  InsertBackupFile limits new -> do
    counts <- gets (epochCounts new.user new.client)
    now <- Now.get
    for (checkUpload limits counts new.epoch new.size) $ \kind -> do
      let file = BackupFile {id = new.id, kind, size = new.size, createdAt = toUTCTimeMillis now}
      modify (Map.insert new.id (new.user, new.client, new.epoch, file))
      pure file
  GetDeviceEpochCounts uid cid -> gets (epochCounts uid cid)
  ListBackupFiles uid cid e ->
    gets $ sortOn (\f -> (f.createdAt, f.id)) . map (\(_, _, _, f) -> f) . Map.elems . Map.filter (matches uid cid e)
  LookupBackupFile uid cid e fid ->
    gets $ fmap (\(_, _, _, f) -> f) . mfilter (matches uid cid e) . Map.lookup fid
  DeleteBackupFileRow fid -> modify (Map.delete fid)
  DeleteEpochRows uid cid e -> deleteWhere (matches uid cid e)
  DeleteClientRows uid cid -> deleteWhere (\(u, c, _, _) -> u == uid && c == cid)
  DeleteUserRows uid -> deleteWhere (\(u, _, _, _) -> u == uid)
  where
    matches uid cid e (u, c, e', _) = u == uid && c == cid && e' == e

    epochCounts uid cid =
      Map.fromListWith (+) . map (\(_, _, e, _) -> (e, 1)) . filter (\(u, c, _, _) -> u == uid && c == cid) . Map.elems

    deleteWhere p = do
      (gone, kept) <- gets (Map.partition p)
      put kept
      pure (Map.keys gone)

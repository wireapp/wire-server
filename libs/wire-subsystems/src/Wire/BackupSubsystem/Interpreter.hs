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

module Wire.BackupSubsystem.Interpreter (interpretBackupSubsystem) where

import Conduit (ConduitT, ResourceT)
import Data.Id
import Data.Map qualified as Map
import Data.Qualified
import Data.Set qualified as Set
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Wire.API.Backup
import Wire.API.Team.Feature
import Wire.API.User (User (userTeam))
import Wire.API.User.Client (UserClients (..))
import Wire.BackupBlobStore
import Wire.BackupStore
import Wire.BackupSubsystem
import Wire.BackupSubsystem.Rules
import Wire.BrigAPIAccess
import Wire.GalleyAPIAccess
import Wire.Sem.Random (Random)
import Wire.Sem.Random qualified as Random

interpretBackupSubsystem ::
  ( Member BackupStore r,
    Member BackupBlobStore r,
    Member BrigAPIAccess r,
    Member GalleyAPIAccess r,
    Member Random r,
    Member (Input BackupLimits) r,
    Member (Error BackupError) r,
    Member (Error BlobUploadError) r
  ) =>
  InterpreterFor BackupSubsystem r
interpretBackupSubsystem = interpret $ \case
  ListEpochs luid zcid cid -> do
    let uid = tUnqualified luid
    checkRead uid zcid cid
    BackupEpochs . Map.keys <$> getDeviceEpochCounts uid cid
  UploadBackup luid zcid cid e len src -> uploadBackupImpl (tUnqualified luid) zcid cid e len src
  ListFiles luid zcid cid e -> do
    let uid = tUnqualified luid
    checkRead uid zcid cid
    files <- listBackupFiles uid cid e
    when (null files) $ throw BackupErrNotFound
    pure (BackupFiles files)
  DownloadUrl luid zcid cid e fid -> do
    let uid = tUnqualified luid
    checkRead uid zcid cid
    _ <- lookupBackupFile uid cid e fid >>= note BackupErrNotFound
    signedDownloadUrl fid
  DeleteEpochBackups luid zcid cid e -> do
    let uid = tUnqualified luid
    checkWrite uid zcid cid
    deleteEpochRows uid cid e >>= deleteBlobs
  DeleteClientBackups luid zcid cid -> do
    let uid = tUnqualified luid
    checkWrite uid zcid cid
    deleteClientRows uid cid >>= deleteBlobs
  InternalDeleteClientBackups uid cid -> deleteClientRows uid cid >>= deleteBlobs
  InternalDeleteUserBackups uid -> deleteUserRows uid >>= deleteBlobs

-- | Upload order matters, because brig deletes a client's backups when the client is removed
-- (see the Haddock at @execDelete@ in "Wire.ClientSubsystem.Interpreter"). If that removal
-- happens while we stream, its delete finds no row yet. So the row is inserted first and the
-- client is looked up again afterwards: if it is gone, we roll back row and blob ourselves.
-- Either the removal sees our row, or we see the removal; no backup outlives its client.
-- This assumes brig client reads are consistent with its writes. Any error after the blob
-- was written rolls back blob (and row, once inserted) via 'rollingBack'.
uploadBackupImpl ::
  ( Member BackupStore r,
    Member BackupBlobStore r,
    Member BrigAPIAccess r,
    Member GalleyAPIAccess r,
    Member Random r,
    Member (Input BackupLimits) r,
    Member (Error BackupError) r,
    Member (Error BlobUploadError) r
  ) =>
  UserId ->
  ClientId ->
  ClientId ->
  BackupEpoch ->
  Word64 ->
  ConduitT () ByteString (ResourceT IO) () ->
  Sem r BackupFile
uploadBackupImpl uid zcid cid e len src = do
  checkWrite uid zcid cid
  tid <- getUsers [uid] >>= note BackupErrFeatureDisabled . (userTeam <=< listToMaybe)
  feat <- getFeatureConfigForTeam @_ @AutomaticBackupsConfig tid
  unless (feat.status == FeatureStatusEnabled) $ throw BackupErrFeatureDisabled
  limits <- input
  counts <- getDeviceEpochCounts uid cid
  either (throw . rejectionError) (const (pure ())) (checkUpload limits counts e len)
  fid <- BackupFileId <$> Random.uuid
  putBlob fid len src `catch` \BlobLengthMismatch -> do
    deleteBlobs [fid]
    throw BackupErrIncompleteBody
  let new = NewBackupFile {id = fid, user = uid, client = cid, epoch = e, size = len}
  rollingBack (deleteBlobs [fid]) $ do
    file <- insertBackupFile limits new >>= either (throw . rejectionError) pure
    rollingBack (deleteBackupFileRow fid) $ do
      stillThere <- Set.member cid <$> clientsOf uid
      unless stillThere $ throw BackupErrForbidden
    pure file

-- | Runs the cleanup if the action fails with a 'BackupError' or 'BlobUploadError', then
-- rethrows the original error. A failing cleanup does not mask it. Exceptions outside the
-- 'Error' effects are not caught.
rollingBack ::
  forall r a.
  (Member (Error BackupError) r, Member (Error BlobUploadError) r) =>
  Sem r () ->
  Sem r a ->
  Sem r a
rollingBack cleanup act =
  act `catch` rethrow @BackupError `catch` rethrow @BlobUploadError
  where
    rethrow :: forall e. (Member (Error e) r) => e -> Sem r a
    rethrow err = void (try @BackupError (try @BlobUploadError cleanup)) >> throw err

rejectionError :: UploadRejection -> BackupError
rejectionError = \case
  EpochNotAllowed -> BackupErrEpochNotAllowed
  TooManyFiles -> BackupErrTooManyFiles
  TooLarge -> BackupErrTooLarge

clientsOf :: (Member BrigAPIAccess r) => UserId -> Sem r (Set ClientId)
clientsOf uid = Map.findWithDefault mempty uid . userClients <$> lookupClients [uid]

-- | Reads are allowed from any current client of the user, for any current client of the user.
checkRead ::
  (Member BrigAPIAccess r, Member (Error BackupError) r) =>
  UserId ->
  ClientId ->
  ClientId ->
  Sem r ()
checkRead uid zcid cid = do
  cs <- clientsOf uid
  unless (zcid `Set.member` cs && cid `Set.member` cs) $ throw BackupErrNotFound

-- | Writes are only allowed by a client for itself.
checkWrite ::
  (Member BrigAPIAccess r, Member (Error BackupError) r) =>
  UserId ->
  ClientId ->
  ClientId ->
  Sem r ()
checkWrite uid zcid cid = do
  when (cid /= zcid) $ throw BackupErrForbidden
  cs <- clientsOf uid
  unless (cid `Set.member` cs) $ throw BackupErrNotFound

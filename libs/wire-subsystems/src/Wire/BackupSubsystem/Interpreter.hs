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
import Data.Json.Util (toUTCTimeMillis)
import Data.Map qualified as Map
import Data.Qualified
import Data.Set qualified as Set
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Resource (Resource, onException)
import Wire.API.Backup
import Wire.API.Team.Feature
import Wire.API.User (User (userTeam))
import Wire.API.User.Client (UserClients (..))
import Wire.BackupBlobStore
import Wire.BackupBlobStore.Key
import Wire.BackupSubsystem
import Wire.BackupSubsystem.Rules
import Wire.BackupSubsystem.UUIDv7
import Wire.BrigAPIAccess
import Wire.GalleyAPIAccess
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now
import Wire.Sem.Random (Random)
import Wire.Sem.Random qualified as Random

interpretBackupSubsystem ::
  ( Member BackupBlobStore r,
    Member BrigAPIAccess r,
    Member GalleyAPIAccess r,
    Member Random r,
    Member Now r,
    Member (Input BackupLimits) r,
    Member (Error BackupError) r,
    Member (Error BlobUploadError) r,
    Member Resource r
  ) =>
  InterpreterFor BackupSubsystem r
interpretBackupSubsystem = interpret $ \case
  ListEpochs luid zcid cid -> do
    let uid = tUnqualified luid
    checkRead uid zcid cid
    BackupEpochs . Set.toAscList . Set.fromList <$> listBlobEpochs uid cid
  UploadBackup luid zcid cid e len src -> uploadBackupImpl (tUnqualified luid) zcid cid e len src
  ListFiles luid zcid cid e -> do
    let uid = tUnqualified luid
    checkRead uid zcid cid
    blobs <- listBlobs uid cid e
    when (null blobs) $ throw BackupErrNotFound
    pure . BackupFiles . map toFile $ sortOn (\b -> (b.kind, b.fileId)) blobs
  DownloadUrl luid zcid cid e kind fid -> do
    let uid = tUnqualified luid
        l = BackupLocation {user = uid, client = cid, epoch = e, kind, fileId = fid}
    checkRead uid zcid cid
    unlessM (blobExists l) $ throw BackupErrNotFound
    signedDownloadUrl l
  DeleteEpochBackups luid zcid cid e -> do
    let uid = tUnqualified luid
    checkWrite uid zcid cid
    deleteBlobs (EpochPrefix uid cid e)
  DeleteClientBackups luid zcid cid -> do
    let uid = tUnqualified luid
    checkWrite uid zcid cid
    deleteBlobs (ClientPrefix uid cid)
  InternalDeleteClientBackups uid cid -> deleteBlobs (ClientPrefix uid cid)
  InternalDeleteUserBackups uid -> deleteBlobs (UserPrefix uid)

toFile :: BackupBlob -> BackupFile
toFile b =
  BackupFile
    { id = b.fileId,
      kind = b.kind,
      size = b.size,
      createdAt = toUTCTimeMillis (uuidV7Time b.fileId.unBackupFileId)
    }

-- | Upload one backup file.
--
-- Ordering invariant: PUT the object first, /then/ ask brig whether the client still exists;
-- if not, delete exactly that version and fail. Brig deletes the client before deleting its
-- backups (see 'Wire.ClientSubsystem.Interpreter.execDelete'), so either brig's prefix
-- deletion lists the object, or this re-check sees the client gone. No orphan either way.
-- This assumes brig client reads are consistent with its writes and the store has strong
-- list-after-write consistency.
uploadBackupImpl ::
  ( Member BackupBlobStore r,
    Member BrigAPIAccess r,
    Member GalleyAPIAccess r,
    Member Random r,
    Member Now r,
    Member (Input BackupLimits) r,
    Member (Error BackupError) r,
    Member (Error BlobUploadError) r,
    Member Resource r
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
  epochs <- Set.fromList <$> listBlobEpochs uid cid
  n <- fromIntegral . length <$> listBlobs uid cid e
  kind <- either (throw . rejectionError) pure (checkUpload limits epochs n e len)
  fid <- BackupFileId <$> (uuidV7 <$> Now.get <*> Random.uuid)
  let l = BackupLocation {user = uid, client = cid, epoch = e, kind, fileId = fid}
  version <-
    putBlob l len src `catch` \BlobLengthMismatch -> do
      deleteBlob l
      throw BackupErrIncompleteBody
  -- 'onException': rolled back on /any/ failure (every 'Error' effect interpreted before
  -- 'Resource', and exceptions), the original failure is surfaced. A failing rollback is
  -- discarded by the 'Resource' interpreter, so it never masks the original failure.
  let recheck = do
        stillThere <- Set.member cid <$> clientsOf uid
        unless stillThere $ throw BackupErrForbidden
  recheck `onException` deleteBlobVersion l version
  pure (toFile BackupBlob {kind, fileId = fid, size = len})

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

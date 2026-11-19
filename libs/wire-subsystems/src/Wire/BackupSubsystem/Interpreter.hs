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
import Data.Json.Util (UTCTimeMillis)
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
  ListUserBackups luid zcid -> do
    let uid = tUnqualified luid
    cs <- clientsOf uid
    unless (zcid `Set.member` cs) $ throw BackupErrNotFound
    clients <- filter (not . null . (.epochs)) <$> traverse (clientBackups uid) (Set.toAscList cs)
    pure . UserBackups $ sortOn (Down . newestBackup) clients
  ListClientBackups luid zcid cid -> do
    let uid = tUnqualified luid
    checkRead uid zcid cid
    clientBackups uid cid
  UploadBackup luid zcid cid e base len src ->
    uploadBackupImpl (tUnqualified luid) zcid cid e base len src
  ListFiles luid zcid cid e -> do
    let uid = tUnqualified luid
    checkRead uid zcid cid
    (full, incrementals) <- listBlobs uid cid e >>= note BackupErrNotFound . currentFiles
    pure BackupFiles {full = toFile full, incrementals = map toFile incrementals}
  DownloadUrl luid zcid cid e fullId incremental -> do
    let uid = tUnqualified luid
        file = maybe (FullFile fullId) (IncrementalFile fullId) incremental
        l = BackupLocation {user = uid, client = cid, epoch = e, file}
    checkRead uid zcid cid
    unlessM (blobExists l) $ throw BackupErrNotFound
    signedDownloadUrl l
  DeleteClientBackups luid zcid cid -> do
    let uid = tUnqualified luid
    checkWrite uid zcid cid
    -- Tombstone first, then delete: see 'uploadBackupImpl'. The deletion keeps what is not older
    -- than 't': files of later uploads, tombstones of concurrent newer deletions.
    t <- newFileId
    putTombstone uid cid t
    deleteClientBackupsBefore uid cid t
  InternalDeleteClientBackups uid cid -> deleteBlobs (ClientPrefix uid cid)
  InternalDeleteUserBackups uid -> deleteBlobs (UserPrefix uid)

-- | Upload one backup file.
--
-- A failed upload leaves nothing behind: everything from the PUT on is guarded and removes the
-- exact object (all versions, the 'VersionId' is unknown if the failure hits after S3 committed
-- but before it was returned) on any failure, including exceptions such as the cancellation of
-- the request when the client disconnects.
--
-- Ordering invariant: PUT the object first, /then/ ask brig whether the client still exists;
-- if not, delete it and fail. Brig deletes the client before deleting its
-- backups (see 'Wire.ClientSubsystem.Interpreter.execDelete'), so either brig's prefix
-- deletion lists the object, or this re-check sees the client gone. No orphan either way.
-- This assumes brig client reads are consistent with its writes and the store has strong
-- list-after-write consistency.
--
-- Backup deletion by the client (see 'DeleteClientBackups') races with uploads of the same
-- client in the same way: it first stores a tombstone (an object @.tombstones/<id>@ with a
-- UUIDv7 id, like file ids), then deletes everything older than the tombstone. A file is as old
-- as its full file ('fullFileId'): incremental files belong to it. After the PUT (and the client
-- check above) an upload lists the tombstones; one newer than the file's full file id means the
-- deletion removes the file, so the upload deletes its object and fails. Either the deletion
-- lists the object, or the upload sees the tombstone. Files of full files started after the
-- deletion are newer, kept by the deletion and pass. This relies on UUIDv7 time order across
-- servers, like "newest full file wins".
--
-- A stored object is complete (S3 makes it visible on completion only), so there is no lock: a
-- new full file replaces the previous one. The newest full file (greatest id) wins. An
-- incremental file is stored under the full file it extends, which must be the newest one, so it
-- cannot belong to a full file that is still uploading. After storing, every upload deletes what
-- the newest full file superseded: older full files, and incremental files under them, also
-- orphans of an incremental upload that raced a newer full file. A full file also deletes all
-- files of all but the two newest epochs (listed after the upload, so epochs created meanwhile
-- count). Concurrent full uploads prune each other, a late older one deletes itself. An upload
-- whose own prune deletes its file fails.
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
  Maybe BackupFileId ->
  Word64 ->
  ConduitT () ByteString (ResourceT IO) () ->
  Sem r BackupFile
uploadBackupImpl uid zcid cid e base len src = do
  checkWrite uid zcid cid
  tid <- getUsers [uid] >>= note BackupErrFeatureDisabled . (userTeam <=< listToMaybe)
  feat <- getFeatureConfigForTeam @_ @AutomaticBackupsConfig tid
  unless (feat.status == FeatureStatusEnabled) $ throw BackupErrFeatureDisabled
  limits <- input
  epochs <- Set.fromList <$> listBlobEpochs uid cid
  blobs <- listBlobs uid cid e
  either (throw . rejectionError) pure $ checkUpload limits base epochs blobs e len
  fid <- newFileId
  let file = maybe (FullFile fid) (`IncrementalFile` fid) base
  store BackupLocation {user = uid, client = cid, epoch = e, file}
  superseded <- pruneTargets <$> listBlobs uid cid e
  traverse_ (deleteBlob . location) superseded
  -- A newer full was stored during the upload: this file was just pruned, don't report it.
  when (file `elem` map (.file) superseded) $ throw BackupErrFullMissing
  -- Epochs listed again: one may have appeared during the upload.
  when (isNothing base) $ do
    stale <- staleEpochs . Set.insert e . Set.fromList <$> listBlobEpochs uid cid
    traverse_ (deleteBlobs . EpochPrefix uid cid) stale
    -- Two newer epochs were stored during the upload: this epoch was just pruned, don't report it.
    when (e `elem` stale) $ throw BackupErrEpochNotAllowed
  pure (toFile BackupBlob {file, size = len})
  where
    location b = BackupLocation {user = uid, client = cid, epoch = e, file = b.file}
    store l = do
      let put = void (putBlob l len src) `catch` \BlobLengthMismatch -> throw BackupErrIncompleteBody
          recheck = do
            stillThere <- Set.member cid <$> clientsOf uid
            unless stillThere $ throw BackupErrForbidden
            tombstones <- listTombstones uid cid
            when (any (> fullFileId l.file) tombstones) $ throw BackupErrForbidden
      -- 'onException': removed on /any/ failure (every 'Error' effect interpreted before
      -- 'Resource', and exceptions), the original failure is surfaced. A failing cleanup is
      -- discarded by the 'Resource' interpreter, so it never masks the original failure.
      (put *> recheck) `onException` deleteBlob l

newFileId :: (Member Now r, Member Random r) => Sem r BackupFileId
newFileId = BackupFileId <$> (uuidV7 <$> Now.get <*> Random.uuid)

rejectionError :: UploadRejection -> BackupError
rejectionError = \case
  EpochNotAllowed -> BackupErrEpochNotAllowed
  FullMissing -> BackupErrFullMissing
  TooManyFiles -> BackupErrTooManyFiles
  TooLarge -> BackupErrTooLarge

-- | The restorable epochs of a client, ascending. Epochs without a full file are left out.
clientBackups :: (Member BackupBlobStore r) => UserId -> ClientId -> Sem r ClientBackups
clientBackups uid cid = do
  epochs <- Set.toAscList . Set.fromList <$> listBlobEpochs uid cid
  summaries <- forM epochs $ \e -> epochSummary e <$> listBlobs uid cid e
  pure ClientBackups {client = cid, epochs = catMaybes summaries}

-- | Creation time of the newest file of a client's backups.
newestBackup :: ClientBackups -> Maybe UTCTimeMillis
newestBackup c = foldr (max . Just . (.createdAt)) Nothing c.epochs

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

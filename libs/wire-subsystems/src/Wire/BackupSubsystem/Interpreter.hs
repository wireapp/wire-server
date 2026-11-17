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
  UploadBackup luid zcid cid e kind len src ->
    uploadBackupImpl (tUnqualified luid) zcid cid e kind len src
  ListFiles luid zcid cid e -> do
    let uid = tUnqualified luid
    checkRead uid zcid cid
    files <- epochFiles <$> listBlobs uid cid e
    when (null files) $ throw BackupErrNotFound
    pure (BackupFiles files)
  DownloadUrl luid zcid cid e kind fid -> do
    let uid = tUnqualified luid
        l = BackupLocation {user = uid, client = cid, epoch = e, kind, fileId = fid}
    checkRead uid zcid cid
    unlessM (blobExists l) $ throw BackupErrNotFound
    -- a full file whose lock still exists is not ready
    when (kind == BackupFull) $ whenM (lockExists l) $ throw BackupErrFullLocked
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
-- Full files additionally take a lock ('withFullLock'), so that at most one full file exists
-- per epoch.
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
  BackupKind ->
  Word64 ->
  ConduitT () ByteString (ResourceT IO) () ->
  Sem r BackupFile
uploadBackupImpl uid zcid cid e kind len src = do
  checkWrite uid zcid cid
  tid <- getUsers [uid] >>= note BackupErrFeatureDisabled . (userTeam <=< listToMaybe)
  feat <- getFeatureConfigForTeam @_ @AutomaticBackupsConfig tid
  unless (feat.status == FeatureStatusEnabled) $ throw BackupErrFeatureDisabled
  limits <- input
  epochs <- Set.fromList <$> listBlobEpochs uid cid
  listing <- listBlobs uid cid e
  let ready = readyBlobs listing
      hasFull = any ((== BackupFull) . (.kind)) ready
  -- an unfinished full is reported as locked, not as missing
  when (kind == BackupIncremental && not hasFull && not (null listing.locks)) $
    throw BackupErrFullLocked
  either (throw . rejectionError) pure $
    checkUpload limits kind epochs hasFull (fromIntegral (length ready)) e len
  fid <- BackupFileId <$> (uuidV7 <$> Now.get <*> Random.uuid)
  let l = BackupLocation {user = uid, client = cid, epoch = e, kind, fileId = fid}
  case kind of
    BackupFull -> do
      unless (null listing.locks) $ throw BackupErrFullLocked
      withFullLock l (store l)
    BackupIncremental -> store l
  pure (toFile True BackupBlob {kind, fileId = fid, size = len})
  where
    store l = do
      let put = void (putBlob l len src) `catch` \BlobLengthMismatch -> throw BackupErrIncompleteBody
          recheck = do
            stillThere <- Set.member cid <$> clientsOf uid
            unless stillThere $ throw BackupErrForbidden
      -- 'onException': removed on /any/ failure (every 'Error' effect interpreted before
      -- 'Resource', and exceptions), the original failure is surfaced. A failing cleanup is
      -- discarded by the 'Resource' interpreter, so it never masks the original failure.
      (put *> recheck) `onException` deleteBlob l

-- | Runs a full upload under the lock of its file id, which also marks the file as not ready.
--
-- 1. Put the lock.
-- 2. List the epoch again: another ready full file means 'BackupErrFullExists', any other lock
--    (including one of a full file that is still locked) means 'BackupErrFullLocked'. With strong
--    list-after-write consistency at most one contender sees only its own lock. A contender that
--    finished before our listing put its file before it deleted its lock, so we see its file.
-- 3. Run the upload. The lock is deleted on any failure, after the upload removed its file.
-- 4. Commit: delete the lock. The file is ready from then on.
--
-- There is no lock timeout (the speed of the client's connection is unknown): after a crash the
-- lock stays, the epoch can't get a new full file and the not ready full is shown to clients
-- until they delete the epoch. Both contenders of a tight race may fail, clients retry.
withFullLock ::
  ( Member BackupBlobStore r,
    Member (Error BackupError) r,
    Member Resource r
  ) =>
  BackupLocation ->
  Sem r () ->
  Sem r ()
withFullLock l upload = do
  putLock l
  let guarded = do
        other <- listBlobs l.user l.client l.epoch
        when (any ((== BackupFull) . (.kind)) (readyBlobs other)) $ throw BackupErrFullExists
        unless (all (== l.fileId) other.locks) $ throw BackupErrFullLocked
        upload
  guarded `onException` deleteLock l
  deleteLock l

rejectionError :: UploadRejection -> BackupError
rejectionError = \case
  EpochNotAllowed -> BackupErrEpochNotAllowed
  FullExists -> BackupErrFullExists
  FullMissing -> BackupErrFullMissing
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

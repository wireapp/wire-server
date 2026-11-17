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
  UploadBackup luid zcid cid e kind len src ->
    uploadBackupImpl (tUnqualified luid) zcid cid e kind len src
  ListFiles luid zcid cid e -> do
    let uid = tUnqualified luid
    checkRead uid zcid cid
    files <- map toFile . currentBlobs <$> listBlobs uid cid e
    when (null files) $ throw BackupErrNotFound
    pure (BackupFiles files)
  DownloadUrl luid zcid cid e kind fid -> do
    let uid = tUnqualified luid
        l = BackupLocation {user = uid, client = cid, epoch = e, kind, fileId = fid}
    checkRead uid zcid cid
    unlessM (blobExists l) $ throw BackupErrNotFound
    signedDownloadUrl l
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
-- A stored object is complete (S3 makes it visible on completion only), so there is no lock: a
-- new full file replaces the previous one. The newest full file (greatest id) wins; after storing,
-- a full file deletes the full file and the incremental files it superseded, and all
-- files of all but the two newest epochs. An incremental file deletes nothing.
-- Concurrent full uploads prune each other, a late older one deletes itself.
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
  blobs <- listBlobs uid cid e
  let current = currentBlobs blobs
      hasFull = any ((== BackupFull) . (.kind)) current
  either (throw . rejectionError) pure $
    checkUpload limits kind epochs hasFull (fromIntegral (length current)) e len
  fid <- BackupFileId <$> (uuidV7 <$> Now.get <*> Random.uuid)
  let l = BackupLocation {user = uid, client = cid, epoch = e, kind, fileId = fid}
  store l
  when (kind == BackupFull) $ do
    listBlobs uid cid e >>= traverse_ (deleteBlob . location) . pruneTargets
    traverse_ (deleteBlobs . EpochPrefix uid cid) . staleEpochs $ Set.insert e epochs
  pure (toFile BackupBlob {kind, fileId = fid, size = len})
  where
    location b = BackupLocation {user = uid, client = cid, epoch = e, kind = b.kind, fileId = b.fileId}
    store l = do
      let put = void (putBlob l len src) `catch` \BlobLengthMismatch -> throw BackupErrIncompleteBody
          recheck = do
            stillThere <- Set.member cid <$> clientsOf uid
            unless stillThere $ throw BackupErrForbidden
      -- 'onException': removed on /any/ failure (every 'Error' effect interpreted before
      -- 'Resource', and exceptions), the original failure is surfaced. A failing cleanup is
      -- discarded by the 'Resource' interpreter, so it never masks the original failure.
      (put *> recheck) `onException` deleteBlob l

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

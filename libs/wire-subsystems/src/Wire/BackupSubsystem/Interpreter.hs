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

module Wire.BackupSubsystem.Interpreter (interpretBackupSubsystem, deleteOrphansNow) where

import Conduit (ConduitT, ResourceT)
import Data.Id
import Data.Map qualified as Map
import Data.Qualified
import Data.Set qualified as Set
import Data.Time.Clock (addUTCTime)
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as Log
import System.Logger.Message qualified as Log
import Wire.API.Backup
import Wire.API.Team.Feature
import Wire.API.User.Client (UserClients (..))
import Wire.BackupObjectStore
import Wire.BackupStore
import Wire.BackupSubsystem
import Wire.BrigAPIAccess (BrigAPIAccess, lookupClients)
import Wire.GalleyAPIAccess (GalleyAPIAccess)
import Wire.GalleyAPIAccess qualified as Galley
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now
import Wire.Sem.Random (Random)
import Wire.Sem.Random qualified as Random

interpretBackupSubsystem ::
  ( Member BackupStore r,
    Member BackupObjectStore r,
    Member BrigAPIAccess r,
    Member GalleyAPIAccess r,
    Member Now r,
    Member Random r,
    Member TinyLog r,
    Member (Input BackupConfig) r,
    Member (Error BackupError) r
  ) =>
  InterpreterFor BackupSubsystem r
interpretBackupSubsystem = interpret $ \case
  UploadFull caller zClient cid epoch len body ->
    uploadFullImpl caller zClient cid epoch len body
  GetEpochFiles caller zClient cid epoch -> do
    uid <- authorize caller zClient
    lookupEpochFiles uid cid epoch >>= maybe (throw BackupErrorNotFound) pure
  DownloadFull caller zClient host cid epoch fid -> do
    uid <- authorize caller zClient
    found <- lookupFull uid cid epoch fid
    unless found $ throw BackupErrorNotFound
    presignGet host fid
  UploadIncremental caller zClient cid epoch full len body ->
    uploadIncrementalImpl caller zClient cid epoch full len body
  DownloadIncremental caller zClient host cid epoch full fid -> do
    uid <- authorize caller zClient
    found <- lookupIncremental uid cid epoch full fid
    unless found $ throw BackupErrorNotFound
    presignGet host fid
  ListBackups caller zClient -> do
    uid <- authorize caller zClient
    BackupList <$> listClientBackups uid Nothing
  GetClientBackups caller zClient cid -> do
    uid <- authorize caller zClient
    listClientBackups uid (Just cid) <&> \case
      [] -> ClientBackups {client = cid, epochs = []}
      entry : _ -> entry
  DeleteClientBackups caller zClient cid -> do
    let uid = tUnqualified caller
    when (zClient /= cid) $ throw BackupErrorForbidden
    ensureClientExists uid zClient
    orphanClient uid cid >>= deleteOrphansNow
  CleanupOrphans -> do
    config <- input
    batch <- selectCleanupBatch config.orphanGracePeriod config.cleanupBatchSize
    -- rows of failed deletions stay, the select already marked them as tried
    gone <- deleteGone batch
    let counts = Log.field "deleted" (length gone) . Log.field "selected" (length batch)
    unless (null batch) $
      Log.info $
        Log.msg (Log.val "Cleaned up orphaned backup files") . counts
    pure (fromIntegral (length gone))

-- | Best effort: delete the objects of the orphans, then their rows. Rows of orphans whose
-- objects are not gone stay (marked as tried) for the cleanup. S3 failures are logged by the
-- object store interpreter and never thrown.
deleteOrphansNow ::
  (Member BackupStore r, Member BackupObjectStore r, Member TinyLog r) =>
  [BackupFileId] ->
  Sem r ()
deleteOrphansNow [] = pure ()
deleteOrphansNow ids = do
  gone <- deleteGone ids
  let goneSet = Set.fromList gone
      failed = filter (`Set.notMember` goneSet) ids
  markDeletionTried failed
  let counts = Log.field "deleted" (length gone) . Log.field "failed" (length failed)
  if null failed
    then Log.debug $ Log.msg (Log.val "Deleted orphaned backup files") . counts
    else Log.warn $ Log.msg (Log.val "Could not delete all orphaned backup files") . counts

-- | Deletes the objects, then the rows of those that are gone. Returns the gone ones.
deleteGone ::
  (Member BackupStore r, Member BackupObjectStore r) =>
  [BackupFileId] ->
  Sem r [BackupFileId]
deleteGone ids = do
  gone <- deleteObjects ids
  deleteOrphanRows gone
  pure gone

uploadFullImpl ::
  ( Member BackupStore r,
    Member BackupObjectStore r,
    Member BrigAPIAccess r,
    Member GalleyAPIAccess r,
    Member Now r,
    Member Random r,
    Member TinyLog r,
    Member (Input BackupConfig) r,
    Member (Error BackupError) r
  ) =>
  Local UserId ->
  ClientId ->
  ClientId ->
  BackupEpoch ->
  Word64 ->
  ConduitT () ByteString (ResourceT IO) () ->
  Sem r BackupFile
uploadFullImpl caller zClient cid epoch len body = do
  config <- input
  uid <- checkUpload caller zClient cid config.maxFullBytes len
  -- cheap, non-authoritative pre-check; 'commitFull' checks again under the lock
  maxEpoch <- lookupMaxEpoch uid cid
  unless (epochAccepted maxEpoch epoch) $ throw BackupErrorEpochInvalid
  fid <- storeUpload uid cid epoch len body
  commitFull uid cid epoch fid len >>= \case
    Left CommitEpochInvalid -> discardUpload fid BackupErrorEpochInvalid
    Left CommitReservationGone -> reservationGone fid
    Right (file, orphaned) -> do
      deleteOrphansNow orphaned
      rejectIfClientGone uid cid
      pure file

uploadIncrementalImpl ::
  ( Member BackupStore r,
    Member BackupObjectStore r,
    Member BrigAPIAccess r,
    Member GalleyAPIAccess r,
    Member Now r,
    Member Random r,
    Member TinyLog r,
    Member (Input BackupConfig) r,
    Member (Error BackupError) r
  ) =>
  Local UserId ->
  ClientId ->
  ClientId ->
  BackupEpoch ->
  BackupFileId ->
  Word64 ->
  ConduitT () ByteString (ResourceT IO) () ->
  Sem r BackupFile
uploadIncrementalImpl caller zClient cid epoch full len body = do
  config <- input
  uid <- checkUpload caller zClient cid config.maxIncrementalBytes len
  -- cheap, non-authoritative pre-checks; 'commitIncremental' checks again under the lock
  countEpochFiles uid cid epoch full >>= \case
    Nothing -> throw BackupErrorFullMissing
    Just n | n >= config.maxFilesPerEpoch -> throw BackupErrorEpochFull
    Just _ -> pure ()
  fid <- storeUpload uid cid epoch len body
  commitIncremental config.maxFilesPerEpoch uid cid epoch full fid len >>= \case
    Left CommitFullMissing -> discardUpload fid BackupErrorFullMissing
    Left CommitEpochFull -> discardUpload fid BackupErrorEpochFull
    Left CommitIncrementalReservationGone -> reservationGone fid
    Right file -> rejectIfClientGone uid cid $> file

-- | Reserves a new file, uploads the body to it and checks the upload deadline. The file is
-- not part of an epoch yet.
storeUpload ::
  ( Member BackupStore r,
    Member BackupObjectStore r,
    Member Now r,
    Member Random r,
    Member TinyLog r,
    Member (Input BackupConfig) r,
    Member (Error BackupError) r
  ) =>
  UserId ->
  ClientId ->
  BackupEpoch ->
  Word64 ->
  ConduitT () ByteString (ResourceT IO) () ->
  Sem r BackupFileId
storeUpload uid cid epoch len body = do
  config <- input
  start <- Now.get
  fid <- BackupFileId <$> Random.uuid
  reserveUpload uid cid epoch fid
  putObject fid len body >>= either (throw . putError) pure
  end <- Now.get
  -- the reservation may have been cleaned up after the grace period
  when (end > addUTCTime config.uploadTimeout start) $ do
    Log.err $ Log.msg (Log.val "Backup upload exceeded the upload timeout, not committing") . Log.field "file" (show fid)
    throw BackupErrorUploadFailed
  pure fid
  where
    putError PutIncompleteBody = BackupErrorIncompleteBody
    putError PutFailed = BackupErrorUploadFailed

-- | The commit rejected the uploaded file: delete it now instead of after the grace period.
discardUpload ::
  ( Member BackupStore r,
    Member BackupObjectStore r,
    Member TinyLog r,
    Member (Error BackupError) r
  ) =>
  BackupFileId ->
  BackupError ->
  Sem r a
discardUpload fid err = deleteOrphansNow [fid] >> throw err

reservationGone :: (Member TinyLog r, Member (Error BackupError) r) => BackupFileId -> Sem r a
reservationGone fid = do
  Log.err $ Log.msg (Log.val "Backup upload reservation vanished before commit") . Log.field "file" (show fid)
  throw BackupErrorUploadFailed

-- | The calling client must exist and the feature must be enabled. Returns the user.
authorize ::
  ( Member BrigAPIAccess r,
    Member GalleyAPIAccess r,
    Member (Error BackupError) r
  ) =>
  Local UserId ->
  ClientId ->
  Sem r UserId
authorize caller zClient = do
  let uid = tUnqualified caller
  ensureClientExists uid zClient
  ensureEnabled uid
  pure uid

-- | Checks of all uploads: only to the own client, authorized, length within the limit.
-- Arguments: caller, calling client (Z-Client), client of the path, limit, Content-Length.
-- Returns the user.
checkUpload ::
  ( Member BrigAPIAccess r,
    Member GalleyAPIAccess r,
    Member (Error BackupError) r
  ) =>
  Local UserId ->
  ClientId ->
  ClientId ->
  Word64 ->
  Word64 ->
  Sem r UserId
checkUpload caller zClient cid maxBytes len = do
  when (zClient /= cid) $ throw BackupErrorForbidden
  uid <- authorize caller zClient
  when (len == 0) $ throw BackupErrorInvalidLength
  when (len > maxBytes) $ throw BackupErrorTooLarge
  pure uid

-- | The calling client must still exist.
ensureClientExists ::
  (Member BrigAPIAccess r, Member (Error BackupError) r) =>
  UserId ->
  ClientId ->
  Sem r ()
ensureClientExists uid zClient =
  clientExists uid zClient >>= (`unless` throw BackupErrorForbidden)

clientExists :: (Member BrigAPIAccess r) => UserId -> ClientId -> Sem r Bool
clientExists uid cid = do
  UserClients clients <- lookupClients [uid]
  pure (cid `elem` Map.findWithDefault mempty uid clients)

-- | Runs after a successful commit. Brig orphans the backups of a client when it removes the
-- client (and again afterwards), but an upload that passed 'ensureClientExists' before the
-- removal can commit its first epoch after brig's orphaning. If the client is gone by now,
-- orphan its backups (this upload's included), delete the objects and fail the upload.
--
-- TODO: This is the only safety net once brig's second orphaning has run. If it does not
-- finish (brig RPC or Postgres error, cargohold dies after the commit), the committed epoch
-- of the removed client is never orphaned: the cleanup job only looks at orphaned rows, so
-- rows and objects stay until the user is deleted, and 'ListBackups' keeps listing the
-- epoch (the user cannot delete it, that needs the removed client as Z-Client). Accepted
-- for now (needs the race and a failure in a small window). Possible fixes: filter listings
-- and downloads by the live clients, or reconcile clients with backups against brig in the
-- cleanup job.
rejectIfClientGone ::
  ( Member BackupStore r,
    Member BackupObjectStore r,
    Member BrigAPIAccess r,
    Member TinyLog r,
    Member (Error BackupError) r
  ) =>
  UserId ->
  ClientId ->
  Sem r ()
rejectIfClientGone uid cid = do
  exists <- clientExists uid cid
  unless exists $ do
    Log.warn $
      Log.msg (Log.val "Client removed during a backup upload, orphaning its backups")
        . Log.field "user" (show uid)
        . Log.field "client" (show cid)
    orphanClient uid cid >>= deleteOrphansNow
    throw BackupErrorForbidden

-- | The user must be in a team with the feature @automaticBackups@ enabled.
ensureEnabled ::
  (Member GalleyAPIAccess r, Member (Error BackupError) r) =>
  UserId ->
  Sem r ()
ensureEnabled uid = do
  tid <- Galley.getTeamId uid >>= maybe (throw BackupErrorDisabled) pure
  feature <- Galley.getFeatureConfigForTeam @_ @AutomaticBackupsConfig tid
  unless (feature.status == FeatureStatusEnabled) $ throw BackupErrorDisabled

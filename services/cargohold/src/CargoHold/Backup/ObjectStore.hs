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

module CargoHold.Backup.ObjectStore (interpretBackupObjectStoreS3) where

import qualified Amazonka
import Amazonka.S3
  ( BucketName (..),
    ObjectKey (..),
    ObjectVersionId (..),
    newCreateMultipartUpload,
    newDelete,
    newDeleteObjects,
    newGetBucketVersioning,
    newListObjectVersions,
    newObjectIdentifier,
  )
import Amazonka.S3.Lens
  ( createMultipartUpload_contentType,
    deleteMarkerEntry_key,
    deleteMarkerEntry_versionId,
    deleteObjectsResponse_errors,
    delete_objects,
    delete_quiet,
    getBucketVersioningResponse_status,
    listObjectVersionsResponse_deleteMarkers,
    listObjectVersionsResponse_isTruncated,
    listObjectVersionsResponse_versions,
    listObjectVersions_prefix,
    objectIdentifier_versionId,
    objectVersion_key,
    objectVersion_versionId,
    s3ServiceError_code,
    s3ServiceError_key,
  )
import CargoHold.API.Error (serverError)
import qualified CargoHold.AWS as AWS
import CargoHold.Backup.Deletion
import CargoHold.S3 (StreamUploadError (..), presignGetObject, selectAwsEnv, streamUploadObject)
import Conduit (fuseBoth, runConduit, runResourceT, sinkList)
import Control.Lens ((.~), (?~), (^.))
import Data.Domain (Domain, domainText)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time.Clock (NominalDiffTime)
import Imports
import Network.HTTP.Types.Status (statusCode)
import qualified Network.Wai.Utilities.Error as Wai
import Polysemy
import Polysemy.Error
import Polysemy.TinyLog (TinyLog)
import qualified Polysemy.TinyLog as Log
import qualified System.Logger as Logger
import System.Logger.Message (field, msg, val)
import qualified System.Logger.Message as Log
import System.Timeout (timeout)
import UnliftIO.Async (pooledMapConcurrentlyN)
import UnliftIO.Exception (tryAny)
import Wire.API.Backup (BackupFileId (..))
import Wire.BackupObjectStore

-- | The upload is cancelled after the given time.
--
-- TODO: Stale multipart uploads. On a timeout (an async exception) the S3 multipart upload
-- is not aborted ('streamUploadObject' only aborts on synchronous errors), like for assets
-- ('CargoHold.S3.uploadV3'). The same happens when cargohold dies during an upload. The
-- already uploaded parts stay in S3 (invisible, but stored and billed). Deleting the key
-- ('DeleteObjects') does not remove them. Currently, we rely on operators configuring an
-- @AbortIncompleteMultipartUpload@ lifecycle rule (see release notes). Reconsider this
-- choice: abort in code (needs the UploadId, which 'SU.streamUpload' hides), or a cleanup
-- via @ListMultipartUploads@ in the cleanup job.
interpretBackupObjectStoreS3 ::
  ( Member (Embed IO) r,
    Member TinyLog r,
    Member (Error Wai.Error) r
  ) =>
  AWS.Env ->
  Map Domain AWS.Env ->
  NominalDiffTime ->
  Word ->
  InterpreterFor BackupObjectStore r
interpretBackupObjectStoreS3 env multiIngress uploadTimeout downloadLinkTTL = interpret $ \case
  PutObject fid len body -> do
    let createReq =
          newCreateMultipartUpload (BucketName env.s3Bucket) (ObjectKey (backupKey fid))
            & createMultipartUpload_contentType ?~ "application/octet-stream"
        micros = ceiling (uploadTimeout * 1000000)
    embed (timeout micros (streamUploadObject env createReq len body)) >>= \case
      Nothing -> do
        Log.err $ Log.msg (Log.val "Backup upload timed out") . Log.field "file" (backupKey fid)
        pure (Left PutFailed)
      Just (Left StreamIncompleteBody) -> pure (Left PutIncompleteBody)
      Just (Left (StreamUploadFailed details)) -> do
        Log.err $ Log.msg details
        pure (Left PutFailed)
      Just (Right ()) -> pure (Right ())
  DeleteObjects ids -> embed (deleteBackupObjects env ids)
  PresignGet host fid -> do
    presignEnv <- case selectAwsEnv env multiIngress host of
      Left err -> do
        Log.warn $
          Log.msg (Log.val "No matching multiIngress config for the Z-Host header")
            . Log.field "host" (maybe "<none>" domainText host)
        throw err
      Right e -> pure e
    presignGetObject presignEnv downloadLinkTTL (backupKey fid) >>= \case
      Right uri -> pure uri
      Left err -> do
        Log.err $ Log.msg (Log.val "Failed to presign backup download") . Log.field "error" err
        throw serverError

-- | Deletes the objects of the files in batches. On versioned (or suspended, or unknown
-- versioning state) buckets all versions and delete markers are deleted, otherwise only the
-- keys. Returns the files whose objects are gone. Failures (also exceptions) are logged and
-- never thrown: the objects of a failed batch are reported as not gone.
deleteBackupObjects :: AWS.Env -> [BackupFileId] -> IO [BackupFileId]
deleteBackupObjects _ [] = pure []
deleteBackupObjects env ids = do
  versioning <- getVersioning env
  listings <- case versioning of
    VersioningNeverEnabled -> pure Map.empty
    VersioningMaybeOn -> do
      noListing <- newIORef False
      Map.fromList
        <$> pooledMapConcurrentlyN s3Parallelism (\i -> (i,) <$> listFile env noListing i) ids
  let plan = planDeletion ids versioning listings
  failed <-
    Set.unions <$> pooledMapConcurrentlyN s3Parallelism (deleteBatch env) (deleteBatches plan)
  pure (goneIds plan failed)

-- | An error (also an exception) means unknown: 'VersioningMaybeOn'.
getVersioning :: AWS.Env -> IO Versioning
getVersioning env = do
  let req = newGetBucketVersioning (BucketName env.s3Bucket)
  tryAny (runResourceT (AWS.sendCatch env.amazonkaEnv req)) >>= \case
    Left e -> unknown "exception" (show e)
    Right (Left e) -> unknown "error" (show e)
    Right (Right resp) -> pure (versioningFrom (Right (resp ^. getBucketVersioningResponse_status)))
  where
    unknown :: Text -> String -> IO Versioning
    unknown what details = do
      Logger.warn env.logger $
        msg (val "Getting bucket versioning failed, listing object versions")
          . field "kind" what
          . field "details" details
      pure VersioningMaybeOn

-- | How many S3 requests (listings of files, deletions of batches) run concurrently.
s3Parallelism :: Int
s3Parallelism = 8

-- | All versions and delete markers with the prefix of the file ('planDeletion' keeps the exact
-- key). If the store does not implement listing, the plain key is the only target and the
-- flag makes the remaining files of the call skip listing.
listFile :: AWS.Env -> IORef Bool -> BackupFileId -> IO Listing
listFile env noListing fid = do
  let plainKey = Listed [ObjectTarget (backupKey fid) Nothing]
  skip <- readIORef noListing
  if skip
    then pure plainKey
    else do
      let req =
            newListObjectVersions (BucketName env.s3Bucket)
              & listObjectVersions_prefix ?~ backupKey fid
          source = Amazonka.paginateEither env.amazonkaEnv req
      tryAny (runResourceT (runConduit (fuseBoth source sinkList))) >>= \case
        Left e -> failed "exception" (show e)
        Right (Left e, _)
          | classifyError e == ListNotImplemented -> do
              writeIORef noListing True
              Logger.warn env.logger $
                msg (val "Store does not support listing object versions, deleting plain keys")
              pure plainKey
          | otherwise -> failed "error" (show e)
        Right (Right (), pages)
          -- the pager stops silently if the continuation markers are missing
          | (listToMaybe (reverse pages) >>= (^. listObjectVersionsResponse_isTruncated)) == Just True ->
              failed "truncated" "last page is truncated"
          | any (isNothing . (.versionId)) exact -> failed "no version id" "entry without version id"
          | otherwise -> pure (Listed targets)
          where
            targets = concatMap pageTargets pages
            exact = filter ((== backupKey fid) . (.key)) targets
  where
    -- a non-AWS answer (e.g. a proxy's plain 501) is a SerializeError: classify by status only
    classifyError (Amazonka.ServiceError se) =
      classifyListError
        (statusCode (se ^. Amazonka.serviceError_status))
        (errorCodeText (se ^. Amazonka.serviceError_code))
    classifyError (Amazonka.SerializeError se) =
      classifyListError (statusCode (se ^. Amazonka.serializeError_status)) ""
    classifyError _ = ListOtherError
    pageTargets page =
      [ ObjectTarget k (fromVersion <$> (v ^. objectVersion_versionId))
      | v <- fromMaybe [] (page ^. listObjectVersionsResponse_versions),
        Just (ObjectKey k) <- [v ^. objectVersion_key]
      ]
        <> [ ObjectTarget k (fromVersion <$> (m ^. deleteMarkerEntry_versionId))
           | m <- fromMaybe [] (page ^. listObjectVersionsResponse_deleteMarkers),
             Just (ObjectKey k) <- [m ^. deleteMarkerEntry_key]
           ]
    fromVersion (ObjectVersionId v) = v
    errorCodeText (Amazonka.ErrorCode c) = c
    failed :: Text -> String -> IO Listing
    failed what details = do
      Logger.warn env.logger $
        msg (val "Listing backup object versions failed")
          . field "file" (backupKey fid)
          . field "kind" what
          . field "details" details
      pure ListingFailed

-- | The targets of the batch that could not be deleted.
deleteBatch :: AWS.Env -> [ObjectTarget] -> IO (Set ObjectTarget)
deleteBatch env batch = do
  let objects = [toIdentifier t | t <- batch]
      toIdentifier t =
        newObjectIdentifier (ObjectKey t.key) & objectIdentifier_versionId .~ (ObjectVersionId <$> t.versionId)
      req =
        newDeleteObjects (BucketName env.s3Bucket) $
          newDelete & delete_objects .~ objects & delete_quiet ?~ True
  tryAny (runResourceT (AWS.sendCatch env.amazonkaEnv req)) >>= \case
    Left e -> wholeBatch "exception" (show e)
    Right (Left e) -> wholeBatch "error" (show e)
    Right (Right resp) -> do
      let errs = fromMaybe [] (resp ^. deleteObjectsResponse_errors)
          errKeys = [k | e <- errs, Just (ObjectKey k) <- [e ^. s3ServiceError_key]]
          -- by key only: an error for a key fails every target with that key
          failed = [t | t <- batch, t.key `elem` errKeys]
      -- an error without a key cannot be attributed to an object
      if length errKeys /= length errs
        then wholeBatch "error without key" (show (map (^. s3ServiceError_code) errs))
        else do
          unless (null errs) $
            Logger.err env.logger $
              msg (val "Deleting backup objects partially failed")
                . field "failed" (length errs)
                . field "code" (show (listToMaybe (map (^. s3ServiceError_code) errs)))
          pure (Set.fromList failed)
  where
    wholeBatch :: Text -> String -> IO (Set ObjectTarget)
    wholeBatch what details = do
      Logger.err env.logger $
        msg (val "Deleting backup objects failed")
          . field "kind" what
          . field "objects" (length batch)
          . field "details" details
      pure (Set.fromList batch)

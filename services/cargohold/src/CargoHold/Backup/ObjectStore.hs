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

module CargoHold.Backup.ObjectStore (interpretBackupObjectStoreS3, deleteBackupObjects) where

import Amazonka.S3
  ( BucketName (..),
    ObjectKey (..),
    ObjectVersionId (..),
    newCreateMultipartUpload,
    newDelete,
    newDeleteObjects,
    newObjectIdentifier,
  )
import Amazonka.S3.Lens
  ( createMultipartUpload_contentType,
    deleteObjectsResponse_errors,
    delete_objects,
    delete_quiet,
    objectIdentifier_versionId,
    s3ServiceError_code,
    s3ServiceError_key,
  )
import CargoHold.API.Error (serverError)
import qualified CargoHold.AWS as AWS
import CargoHold.Backup.Deletion
import CargoHold.S3 (presignGetObject, selectAwsEnv, streamUploadObject)
import Conduit (runResourceT)
import Control.Lens ((.~), (?~), (^.))
import Data.Domain (Domain, domainText)
import qualified Data.Set as Set
import Data.Time.Clock (NominalDiffTime)
import Imports
import qualified Network.Wai.Utilities.Error as Wai
import Polysemy
import Polysemy.Error
import Polysemy.TinyLog (TinyLog)
import qualified Polysemy.TinyLog as Log
import qualified System.Logger as Logger
import System.Logger.Message (field, msg, val)
import qualified System.Logger.Message as Log
import System.Timeout (timeout)
import UnliftIO.Exception (tryAny)
import Wire.API.Backup (BackupFileId (..))
import Wire.BackupObjectStore

-- | The upload is cancelled after the given time. Unfinished multipart uploads are
-- aborted by 'streamUploadObject'.
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
        pure (Left PutTimedOut)
      Just (Left (PutIncompleteBody, _)) -> pure (Left PutIncompleteBody)
      Just (Left (err, details)) -> do
        Log.err $ Log.msg details
        pure (Left err)
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

-- | Deletes the objects of the files (keys only, no versions) in batches. Returns the files
-- whose objects are gone. Failures (also exceptions) are logged and never thrown: the objects
-- of a failed batch are reported as not gone.
deleteBackupObjects :: AWS.Env -> [BackupFileId] -> IO [BackupFileId]
deleteBackupObjects env ids = do
  let plan = keysOnlyPlan ids
  failed <- traverse (deleteBatch env) (deleteBatches plan)
  pure (goneIds plan (Set.unions failed))

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

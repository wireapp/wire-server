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

module CargoHold.Backup.ObjectStore (interpretBackupObjectStoreS3, backupKey) where

import Amazonka.S3 (BucketName (..), ObjectKey (..), newCreateMultipartUpload)
import Amazonka.S3.Lens (createMultipartUpload_contentType)
import qualified CargoHold.AWS as AWS
import CargoHold.API.Error (serverError)
import CargoHold.S3 (presignGetObject, selectAwsEnv, streamUploadObject)
import Control.Lens ((?~))
import Data.Domain (Domain)
import Data.Time.Clock (NominalDiffTime)
import qualified Data.UUID as UUID
import Imports
import qualified Network.Wai.Utilities.Error as Wai
import Polysemy
import Polysemy.Error
import Polysemy.TinyLog (TinyLog)
import qualified Polysemy.TinyLog as Log
import qualified System.Logger.Message as Log
import System.Timeout (timeout)
import Wire.API.Backup (BackupFileId (..))
import Wire.BackupObjectStore

-- | S3 key of a backup file.
backupKey :: BackupFileId -> Text
backupKey (BackupFileId u) = "backups/" <> UUID.toText u

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
  PresignGet host fid -> do
    presignEnv <- either throw pure (selectAwsEnv env multiIngress host)
    presignGetObject presignEnv downloadLinkTTL (backupKey fid) >>= \case
      Right uri -> pure uri
      Left err -> do
        Log.err $ Log.msg (Log.val "Failed to presign backup download") . Log.field "error" err
        throw serverError

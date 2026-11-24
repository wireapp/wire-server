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
import CargoHold.S3 (streamUploadObject)
import Control.Lens ((?~))
import Data.Time.Clock (NominalDiffTime)
import qualified Data.UUID as UUID
import Imports
import Polysemy
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
  (Member (Embed IO) r, Member TinyLog r) =>
  AWS.Env ->
  NominalDiffTime ->
  InterpreterFor BackupObjectStore r
interpretBackupObjectStoreS3 env uploadTimeout = interpret $ \case
  PutObject fid len body -> do
    let createReq =
          newCreateMultipartUpload (BucketName env.s3Bucket) (ObjectKey (backupKey fid))
            & createMultipartUpload_contentType ?~ "application/octet-stream"
        micros = ceiling (uploadTimeout * 1000000)
    embed (timeout micros (streamUploadObject env createReq len body)) >>= \case
      Nothing -> pure (Left PutTimedOut)
      Just (Left (PutIncompleteBody, _)) -> pure (Left PutIncompleteBody)
      Just (Left (err, details)) -> do
        Log.err $ Log.msg details
        pure (Left err)
      Just (Right ()) -> pure (Right ())

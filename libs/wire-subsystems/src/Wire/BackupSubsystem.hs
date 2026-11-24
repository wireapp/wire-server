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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Wire.BackupSubsystem where

import Conduit (ResourceT)
import Data.Aeson (FromJSON (..), withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (JSONPathElement (Key), parserThrowError)
import Data.Conduit (ConduitT)
import Data.Domain (Domain)
import Data.Id
import Data.Qualified
import Data.Time.Clock (NominalDiffTime)
import Imports
import Polysemy
import URI.ByteString (URI)
import Wire.API.Backup

data BackupConfig = BackupConfig
  { -- | Maximal size of a full backup file in bytes.
    maxFullBytes :: Word64,
    -- | Maximal size of an incremental backup file in bytes.
    maxIncrementalBytes :: Word64,
    -- | Maximal number of files of an epoch, counting the full.
    maxFilesPerEpoch :: Word32,
    -- | Maximal duration of an upload.
    uploadTimeout :: NominalDiffTime,
    -- | Age after which an orphan that is not a deleted file (i.e. an upload reservation) is
    -- collected by the cleanup. Must be greater than 'uploadTimeout'.
    orphanGracePeriod :: NominalDiffTime,
    -- | Maximal number of orphans one cleanup call handles.
    cleanupBatchSize :: Word32
  }
  deriving (Eq, Show)

-- | @uploadTimeout@ and @orphanGracePeriod@ are given in seconds.
instance FromJSON BackupConfig where
  parseJSON = withObject "BackupConfig" $ \o -> do
    maxFullBytes <- o .: "maxFullBytes"
    maxIncrementalBytes <- o .: "maxIncrementalBytes"
    maxFilesPerEpoch <- o .: "maxFilesPerEpoch"
    -- the full counts as a file of its epoch
    when (maxFilesPerEpoch < 1) $ fail "maxFilesPerEpoch must be at least 1"
    uploadTimeout <- fromInteger <$> o .: "uploadTimeout"
    -- 0 fails every upload, a negative value disables the S3 timeout ('System.Timeout.timeout')
    when (uploadTimeout <= 0) $
      parserThrowError [Key "uploadTimeout"] "uploadTimeout must be at least 1"
    orphanGracePeriod <- fromInteger <$> o .:? "orphanGracePeriod" .!= 3600
    -- else the cleanup could remove the reservation of an upload that is still running
    when (orphanGracePeriod <= uploadTimeout) $
      parserThrowError [Key "orphanGracePeriod"] "orphanGracePeriod must be greater than uploadTimeout"
    cleanupBatchSize <- o .:? "cleanupBatchSize" .!= 100
    when (cleanupBatchSize < 1) $
      parserThrowError [Key "cleanupBatchSize"] "cleanupBatchSize must be at least 1"
    pure BackupConfig {..}

data BackupError
  = BackupErrorForbidden
  | BackupErrorDisabled
  | BackupErrorEpochInvalid
  | BackupErrorNotFound
  | BackupErrorTooLarge
  | BackupErrorInvalidLength
  | BackupErrorIncompleteBody
  | BackupErrorUploadFailed
  | BackupErrorFullMissing
  | BackupErrorEpochFull
  deriving (Eq, Show)

data BackupSubsystem m a where
  -- | Arguments: caller, calling client (Z-Client), client of the path, epoch,
  -- Content-Length and body.
  UploadFull ::
    Local UserId ->
    ClientId ->
    ClientId ->
    BackupEpoch ->
    Word64 ->
    ConduitT () ByteString (ResourceT IO) () ->
    BackupSubsystem m BackupFile
  -- | Arguments: caller, calling client (Z-Client), client of the path, epoch.
  GetEpochFiles :: Local UserId -> ClientId -> ClientId -> BackupEpoch -> BackupSubsystem m BackupFiles
  -- | Arguments: caller, calling client (Z-Client), Z-Host, client of the path, epoch, full.
  -- Returns a presigned download URL.
  DownloadFull ::
    Local UserId ->
    ClientId ->
    Maybe Domain ->
    ClientId ->
    BackupEpoch ->
    BackupFileId ->
    BackupSubsystem m URI
  -- | Arguments: caller, calling client (Z-Client), client of the path, epoch, full,
  -- Content-Length and body.
  UploadIncremental ::
    Local UserId ->
    ClientId ->
    ClientId ->
    BackupEpoch ->
    BackupFileId ->
    Word64 ->
    ConduitT () ByteString (ResourceT IO) () ->
    BackupSubsystem m BackupFile
  -- | Arguments: caller, calling client (Z-Client), Z-Host, client of the path, epoch, full,
  -- incremental. Returns a presigned download URL.
  DownloadIncremental ::
    Local UserId ->
    ClientId ->
    Maybe Domain ->
    ClientId ->
    BackupEpoch ->
    BackupFileId ->
    BackupFileId ->
    BackupSubsystem m URI
  -- | Arguments: caller, calling client (Z-Client).
  ListBackups :: Local UserId -> ClientId -> BackupSubsystem m BackupList
  -- | Arguments: caller, calling client (Z-Client), client of the path. Unknown clients, also
  -- those of other users, have no epochs.
  GetClientBackups :: Local UserId -> ClientId -> ClientId -> BackupSubsystem m ClientBackups
  -- | Arguments: caller, calling client (Z-Client), client of the path. Neither the feature
  -- nor a team is required.
  DeleteClientBackups :: Local UserId -> ClientId -> ClientId -> BackupSubsystem m ()
  -- | One batch of the cleanup of orphans: collects reservations older than the grace period
  -- and orphans whose deletion failed before, deletes their objects and rows. Returns the
  -- number of deleted orphans. Internal, no checks.
  CleanupOrphans :: BackupSubsystem m Word32

makeSem ''BackupSubsystem

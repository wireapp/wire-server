{-# LANGUAGE DeriveAnyClass #-}

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

-- | Pure rules for automatic backups. The subsystem calls them with the epoch set and the
-- epoch listing taken from S3. Not atomic with the upload; races are tolerated (spec,
-- "Races"): the newest full file (by id) wins and every upload prunes what it superseded.
module Wire.BackupSubsystem.Rules
  ( BackupLimits (..),
    UploadRejection (..),
    checkUpload,
    currentBlobs,
    pruneTargets,
    staleEpochs,
    toFile,
  )
where

import Data.Aeson (FromJSON)
import Data.Json.Util (toUTCTimeMillis)
import Data.Set qualified as Set
import Imports
import Wire.API.Backup
import Wire.BackupBlobStore
import Wire.BackupSubsystem.UUIDv7

data BackupLimits = BackupLimits
  { -- | Size cap of a full file.
    maxFullBytes :: Word64,
    -- | Size cap of an incremental file.
    maxIncrementalBytes :: Word64,
    maxFilesPerEpoch :: Word32
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data UploadRejection = EpochNotAllowed | FullMissing | TooManyFiles | TooLarge
  deriving stock (Eq, Show)

-- | Decide whether a file of the given kind and size may be stored at the given epoch, given the
-- stored epochs, whether the target epoch has a full file and the number of its current files
-- (all from S3 listings). A full file replaces the previous one, so it is never rejected for an
-- existing one.
checkUpload ::
  BackupLimits ->
  BackupKind ->
  -- | Stored epochs of the device
  Set BackupEpoch ->
  -- | Does the target epoch have a full file?
  Bool ->
  -- | Number of current files in the target epoch, see 'currentBlobs'
  Word32 ->
  BackupEpoch ->
  Word64 ->
  Either UploadRejection ()
checkUpload limits kind epochs hasFull files e size = do
  unless (e.unBackupEpoch >= 1 && epochAllowed (Set.lookupMax epochs)) $ Left EpochNotAllowed
  case kind of
    BackupFull -> do
      when (size > limits.maxFullBytes) $ Left TooLarge
    BackupIncremental -> do
      unless hasFull $ Left FullMissing
      when (size > limits.maxIncrementalBytes) $ Left TooLarge
      unless (files < limits.maxFilesPerEpoch) $ Left TooManyFiles
  where
    -- only the newest stored epoch or its successor
    epochAllowed :: Maybe BackupEpoch -> Bool
    epochAllowed = maybe True (\n -> e == n || Just e == next n)

    next :: BackupEpoch -> Maybe BackupEpoch
    next (BackupEpoch n)
      | n < maxBound = Just (BackupEpoch (n + 1))
      | otherwise = Nothing

-- | The files of an epoch that count: the newest full file (greatest id) and the incremental
-- files after it (greater ids), full first, then incremental, each by id. Empty if there is no
-- full file: incremental files before the newest full file belong to a superseded one.
currentBlobs :: [BackupBlob] -> [BackupBlob]
currentBlobs blobs = case newestFull of
  Nothing -> []
  Just f -> f : sortOn (.fileId) (filter (\b -> b.kind == BackupIncremental && b.fileId > f.fileId) blobs)
  where
    newestFull = listToMaybe . sortOn (Down . (.fileId)) $ filter ((== BackupFull) . (.kind)) blobs

-- | The files of an epoch that a stored full file superseded, to be deleted.
pruneTargets :: [BackupBlob] -> [BackupBlob]
pruneTargets blobs
  | any ((== BackupFull) . (.kind)) blobs = filter (`notElem` currentBlobs blobs) blobs
  | otherwise = []

-- | The epochs to delete: all but the two newest.
staleEpochs :: Set BackupEpoch -> [BackupEpoch]
staleEpochs = reverse . drop 2 . reverse . Set.toAscList

toFile :: BackupBlob -> BackupFile
toFile b =
  BackupFile
    { id = b.fileId,
      kind = b.kind,
      size = b.size,
      createdAt = toUTCTimeMillis (uuidV7Time b.fileId.unBackupFileId)
    }

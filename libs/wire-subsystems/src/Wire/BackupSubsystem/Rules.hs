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
-- "Races"): the newest full file (by id) wins, incremental files belong to the full file they
-- name, and every upload prunes what it superseded.
module Wire.BackupSubsystem.Rules
  ( BackupLimits (..),
    UploadRejection (..),
    checkUpload,
    currentBlobs,
    currentFiles,
    epochSummary,
    pruneTargets,
    staleEpochs,
    toFile,
  )
where

import Data.Aeson (FromJSON)
import Data.Json.Util (toUTCTimeMillis)
import Data.List.Extra (dropEnd)
import Data.Set qualified as Set
import Imports
import Wire.API.Backup
import Wire.BackupBlobStore
import Wire.BackupBlobStore.Key (BackupFileRef (..), fileRefId)
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

-- | Decide whether a file may be stored at the given epoch, given the stored epochs and the
-- files of the target epoch (both from S3 listings). The 'Maybe' is the full file an
-- incremental file extends, 'Nothing' for a full file. A full file replaces the previous one, so
-- it is never rejected for an existing one.
checkUpload ::
  BackupLimits ->
  Maybe BackupFileId ->
  -- | Stored epochs of the device
  Set BackupEpoch ->
  -- | Files of the target epoch
  [BackupBlob] ->
  BackupEpoch ->
  Word64 ->
  Either UploadRejection ()
checkUpload limits base epochs blobs e size = do
  unless (e.unBackupEpoch >= 1 && epochAllowed (Set.lookupMax epochs)) $ Left EpochNotAllowed
  case base of
    Nothing -> when (size > limits.maxFullBytes) $ Left TooLarge
    Just f -> do
      let current = currentBlobs blobs
      -- the newest full file only: older ones are superseded, and pruned
      unless (fmap (fileRefId . (.file)) (listToMaybe current) == Just f) $ Left FullMissing
      when (size > limits.maxIncrementalBytes) $ Left TooLarge
      unless (fromIntegral (length current) < limits.maxFilesPerEpoch) $ Left TooManyFiles
  where
    -- only the newest stored epoch or its successor
    epochAllowed :: Maybe BackupEpoch -> Bool
    epochAllowed = maybe True (\n -> e == n || Just e == next n)

    next :: BackupEpoch -> Maybe BackupEpoch
    next (BackupEpoch n)
      | n < maxBound = Just (BackupEpoch (n + 1))
      | otherwise = Nothing

-- | The newest full file (greatest id) of an epoch and the incremental files stored under it, by
-- id. 'Nothing' if there is no full file. Incremental files under an older full file belong to a
-- superseded one, whatever their ids.
currentFiles :: [BackupBlob] -> Maybe (BackupBlob, [BackupBlob])
currentFiles blobs = do
  f <- listToMaybe (sortOn (Down . fileRefId . (.file)) [b | b@BackupBlob {file = FullFile _} <- blobs])
  let under = [b | b@BackupBlob {file = IncrementalFile full _} <- blobs, full == fileRefId f.file]
  pure (f, sortOn (fileRefId . (.file)) under)

-- | The files of an epoch that count, full first, see 'currentFiles'. Empty without a full file.
currentBlobs :: [BackupBlob] -> [BackupBlob]
currentBlobs = maybe [] (uncurry (:)) . currentFiles

-- | Summary of the current files ('currentBlobs') of an epoch, 'Nothing' if it has none.
epochSummary :: BackupEpoch -> [BackupBlob] -> Maybe EpochSummary
epochSummary e blobs = case map toFile (currentBlobs blobs) of
  [] -> Nothing
  files ->
    Just
      EpochSummary
        { epoch = e,
          createdAt = maximum (map (.createdAt) files),
          fileCount = fromIntegral (length files),
          size = sum (map (.size) files)
        }

-- | The files of an epoch that a stored full file superseded or that were stored under a
-- superseded full file, to be deleted.
pruneTargets :: [BackupBlob] -> [BackupBlob]
pruneTargets blobs = case currentBlobs blobs of
  [] -> []
  current -> filter (`notElem` current) blobs

-- | The epochs to delete: all but the two newest.
staleEpochs :: Set BackupEpoch -> [BackupEpoch]
staleEpochs = dropEnd 2 . Set.toAscList

toFile :: BackupBlob -> BackupFile
toFile b =
  BackupFile
    { id = fileRefId b.file,
      size = b.size,
      createdAt = toUTCTimeMillis (uuidV7Time (fileRefId b.file).unBackupFileId)
    }

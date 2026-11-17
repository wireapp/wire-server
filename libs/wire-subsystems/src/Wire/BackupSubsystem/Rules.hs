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
-- epoch listing taken from S3. Not atomic with the upload; races of incrementals are tolerated
-- (spec, "Races"), full uploads exclude each other with a lock (see
-- 'Wire.BackupSubsystem.Interpreter').
module Wire.BackupSubsystem.Rules
  ( BackupLimits (..),
    UploadRejection (..),
    checkUpload,
    readyBlobs,
    epochFiles,
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

data UploadRejection = EpochNotAllowed | FullExists | FullMissing | TooManyFiles | TooLarge
  deriving stock (Eq, Show)

-- | Decide whether a file of the given kind and size may be stored at the given epoch, given the
-- stored epochs, whether the target epoch has a ready full file and the number of its ready
-- files (all from S3 listings).
checkUpload ::
  BackupLimits ->
  BackupKind ->
  -- | Stored epochs of the device
  Set BackupEpoch ->
  -- | Does the target epoch have a ready full file?
  Bool ->
  -- | Number of ready files in the target epoch
  Word32 ->
  BackupEpoch ->
  Word64 ->
  Either UploadRejection ()
checkUpload limits kind epochs hasFull files e size = do
  unless (e.unBackupEpoch >= 1 && epochAllowed (Set.toAscList epochs)) $ Left EpochNotAllowed
  case kind of
    BackupFull -> do
      when hasFull $ Left FullExists
      when (size > limits.maxFullBytes) $ Left TooLarge
    BackupIncremental -> do
      unless hasFull $ Left FullMissing
      when (size > limits.maxIncrementalBytes) $ Left TooLarge
      unless (files < limits.maxFilesPerEpoch) $ Left TooManyFiles
  where
    epochAllowed :: [BackupEpoch] -> Bool
    epochAllowed [] = True
    epochAllowed [n] = e == n || Just e == next n
    epochAllowed [n, m] | Just m == next n = e == m
    epochAllowed _ = False

    next :: BackupEpoch -> Maybe BackupEpoch
    next (BackupEpoch n)
      | n < maxBound = Just (BackupEpoch (n + 1))
      | otherwise = Nothing

-- | The files of an epoch that are complete: all but the full files whose lock still exists.
readyBlobs :: BackupEpochListing -> [BackupBlob]
readyBlobs l = filter (\b -> not (isLockedFull l b)) l.blobs

isLockedFull :: BackupEpochListing -> BackupBlob -> Bool
isLockedFull l b = b.kind == BackupFull && b.fileId `elem` l.locks

-- | All files of an epoch as shown to clients: full files first, then incremental ones, each by
-- id. A full file whose lock exists is 'ready = False'. A lock without a file (upload started,
-- nothing stored yet) is shown as a not ready full file of size 0.
epochFiles :: BackupEpochListing -> [BackupFile]
epochFiles l =
  map snd . sortOn fst $
    [((b.kind, b.fileId), toFile (not (isLockedFull l b)) b) | b <- l.blobs]
      <> [ ((BackupFull, fid), toFile False BackupBlob {kind = BackupFull, fileId = fid, size = 0})
         | fid <- l.locks,
           fid `notElem` [b.fileId | b <- l.blobs, b.kind == BackupFull]
         ]

toFile :: Bool -> BackupBlob -> BackupFile
toFile ready b =
  BackupFile
    { id = b.fileId,
      kind = b.kind,
      size = b.size,
      createdAt = toUTCTimeMillis (uuidV7Time b.fileId.unBackupFileId),
      ready
    }

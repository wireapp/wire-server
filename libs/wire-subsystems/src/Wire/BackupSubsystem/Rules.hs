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

-- | Pure upload rules for automatic backups, shared by the store interpreters (which apply them
-- atomically with the insert) and the subsystem.
module Wire.BackupSubsystem.Rules
  ( BackupLimits (..),
    UploadRejection (..),
    checkUpload,
  )
where

import Data.Aeson (FromJSON)
import Data.Map qualified as Map
import Imports
import Wire.API.Backup

data BackupLimits = BackupLimits
  { -- | Size cap of the first file of a @(device, epoch)@.
    maxFullBytes :: Word64,
    -- | Size cap of any later file of a @(device, epoch)@.
    maxIncrementalBytes :: Word64,
    maxFilesPerEpoch :: Word32
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data UploadRejection = EpochNotAllowed | TooManyFiles | TooLarge
  deriving stock (Eq, Show)

-- | Decide whether a file of the given size may be stored at the given epoch, given the file count
-- per stored epoch of the device. Returns the kind of the new file: full iff it is the first file
-- of its epoch.
checkUpload ::
  BackupLimits ->
  Map BackupEpoch Word32 ->
  BackupEpoch ->
  Word64 ->
  Either UploadRejection BackupKind
checkUpload limits counts e size = do
  unless (e.unBackupEpoch >= 1 && epochAllowed (Map.keys counts)) $ Left EpochNotAllowed
  let n = Map.findWithDefault 0 e counts
      (kind, cap)
        | n == 0 = (BackupFull, limits.maxFullBytes)
        | otherwise = (BackupIncremental, limits.maxIncrementalBytes)
  when (size > cap) $ Left TooLarge
  unless (n < limits.maxFilesPerEpoch) $ Left TooManyFiles
  pure kind
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

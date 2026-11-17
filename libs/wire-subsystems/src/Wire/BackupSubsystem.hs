{-# LANGUAGE TemplateHaskell #-}

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

-- | Automatic backups of client-side message databases.
module Wire.BackupSubsystem where

import Conduit (ConduitT, ResourceT)
import Data.Id (ClientId, UserId)
import Data.Qualified (Local)
import Imports
import Network.Wai.Utilities.Error qualified as Wai
import Polysemy
import URI.ByteString (URI)
import Wire.API.Backup
import Wire.API.Error (errorToWai)
import Wire.API.Error.Cargohold qualified as E

data BackupError
  = BackupErrNotFound
  | BackupErrForbidden
  | BackupErrEpochNotAllowed
  | BackupErrTooManyFiles
  | BackupErrTooLarge
  | BackupErrFeatureDisabled
  | BackupErrIncompleteBody
  | BackupErrFullMissing
  deriving stock (Eq, Show)

backupErrorToWai :: BackupError -> Wai.Error
backupErrorToWai = \case
  BackupErrNotFound -> errorToWai @E.BackupNotFound
  BackupErrForbidden -> errorToWai @E.BackupForbidden
  BackupErrEpochNotAllowed -> errorToWai @E.BackupEpochNotAllowed
  BackupErrTooManyFiles -> errorToWai @E.BackupTooManyFiles
  BackupErrTooLarge -> errorToWai @E.BackupTooLarge
  BackupErrFeatureDisabled -> errorToWai @E.BackupFeatureDisabled
  BackupErrIncompleteBody -> errorToWai @E.IncompleteBody
  BackupErrFullMissing -> errorToWai @E.BackupFullMissing

-- | Public operations take the caller (user and @Z-Client@) and the target client @cid@.
data BackupSubsystem m a where
  -- | Takes the caller (user and @Z-Client@) only.
  ListUserBackups :: Local UserId -> ClientId -> BackupSubsystem m UserBackups
  ListClientBackups :: Local UserId -> ClientId -> ClientId -> BackupSubsystem m ClientBackups
  UploadBackup ::
    Local UserId ->
    ClientId ->
    ClientId ->
    BackupEpoch ->
    -- | The full file an incremental file extends, 'Nothing' for a full file
    Maybe BackupFileId ->
    -- | Declared @Content-Length@
    Word64 ->
    ConduitT () ByteString (ResourceT IO) () ->
    BackupSubsystem m BackupFile
  ListFiles :: Local UserId -> ClientId -> ClientId -> BackupEpoch -> BackupSubsystem m BackupFiles
  DownloadUrl ::
    Local UserId ->
    ClientId ->
    ClientId ->
    BackupEpoch ->
    -- | The full file
    BackupFileId ->
    -- | The incremental file under it, 'Nothing' for the full file itself
    Maybe BackupFileId ->
    BackupSubsystem m URI
  InternalDeleteClientBackups :: UserId -> ClientId -> BackupSubsystem m ()
  InternalDeleteUserBackups :: UserId -> BackupSubsystem m ()

makeSem ''BackupSubsystem

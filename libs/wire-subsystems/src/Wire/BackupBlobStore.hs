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

module Wire.BackupBlobStore where

import Conduit (ConduitT, ResourceT)
import Data.Id (ClientId, UserId)
import Imports
import Polysemy
import URI.ByteString (URI)
import Wire.API.Backup
import Wire.BackupBlobStore.Key (BackupLocation (..), BackupPrefix (..))

data BlobUploadError = BlobLengthMismatch
  deriving stock (Eq, Show)

-- | A stored backup file as seen in a listing.
data BackupBlob = BackupBlob
  { kind :: BackupKind,
    fileId :: BackupFileId,
    size :: Word64
  }
  deriving stock (Eq, Show)

-- | S3 operations on backup objects.
--
-- Deletes remove /all versions/: on a versioned bucket a plain delete only adds a delete
-- marker, the encrypted data stays and is restorable. The RFC requires backups of a removed
-- device to be deleted immediately.
data BackupBlobStore m a where
  -- | The 'Word64' is the declared @Content-Length@. Interpreters fail with
  -- 'BlobLengthMismatch' if the stream has a different length. Returns the S3 @VersionId@
  -- of the new object ('Nothing' if the bucket is unversioned).
  PutBlob ::
    BackupLocation ->
    Word64 ->
    ConduitT () ByteString (ResourceT IO) () ->
    BackupBlobStore m (Maybe Text)
  BlobExists :: BackupLocation -> BackupBlobStore m Bool
  SignedDownloadUrl :: BackupLocation -> BackupBlobStore m URI
  -- | Epochs with at least one stored file, any order.
  ListBlobEpochs :: UserId -> ClientId -> BackupBlobStore m [BackupEpoch]
  -- | Files of an epoch, any order.
  ListBlobs :: UserId -> ClientId -> BackupEpoch -> BackupBlobStore m [BackupBlob]
  -- | Deletes exactly the given version, as returned by 'PutBlob'. 'Nothing' is a plain
  -- delete (only correct if the bucket is unversioned). Idempotent.
  DeleteBlobVersion :: BackupLocation -> Maybe Text -> BackupBlobStore m ()
  -- | Deletes all versions of the key (see above why all). Idempotent.
  DeleteBlob :: BackupLocation -> BackupBlobStore m ()
  -- | Deletes all versions of all keys under the prefix (see above why all). Idempotent.
  DeleteBlobs :: BackupPrefix -> BackupBlobStore m ()

makeSem ''BackupBlobStore

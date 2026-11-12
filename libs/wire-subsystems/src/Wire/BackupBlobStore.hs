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

-- | Blob storage of automatic backup files.
module Wire.BackupBlobStore where

import Conduit (ConduitT, ResourceT)
import Imports
import Polysemy
import URI.ByteString (URI)
import Wire.API.Backup

-- | Streamed length differed from the declared length.
data BlobUploadError = BlobLengthMismatch
  deriving stock (Eq, Show)

data BackupBlobStore m a where
  -- | The 'Word64' is the declared @Content-Length@. Interpreters fail with
  -- 'BlobLengthMismatch' if the stream has a different length.
  PutBlob ::
    BackupFileId ->
    Word64 ->
    ConduitT () ByteString (ResourceT IO) () ->
    BackupBlobStore m ()
  SignedDownloadUrl :: BackupFileId -> BackupBlobStore m URI
  -- | Idempotent, missing keys are ignored.
  DeleteBlobs :: [BackupFileId] -> BackupBlobStore m ()

makeSem ''BackupBlobStore

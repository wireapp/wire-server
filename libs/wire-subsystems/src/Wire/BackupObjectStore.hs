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
{-# LANGUAGE TemplateHaskell #-}

module Wire.BackupObjectStore where

import Conduit (ResourceT)
import Data.Conduit (ConduitT)
import Data.Domain (Domain)
import Imports
import Polysemy
import URI.ByteString (URI)
import Wire.API.Backup

data PutObjectError
  = -- | The body was shorter than announced.
    PutIncompleteBody
  | -- | Any other failure (also a timeout), logged by the interpreter.
    PutFailed
  deriving (Eq, Show)

data BackupObjectStore m a where
  -- | Stream a body of the announced size into the object of the given file.
  PutObject ::
    BackupFileId ->
    Word64 ->
    ConduitT () ByteString (ResourceT IO) () ->
    BackupObjectStore m (Either PutObjectError ())
  -- | Presigned download URL of the object of the given file. The 'Domain' selects the
  -- ingress (multi-ingress setups).
  PresignGet :: Maybe Domain -> BackupFileId -> BackupObjectStore m URI
  -- | Delete the objects of the given files. Returns the ids whose objects are gone; a
  -- missing object counts as gone. Never throws: failures are logged by the interpreter
  -- and those ids are not returned.
  DeleteObjects :: [BackupFileId] -> BackupObjectStore m [BackupFileId]

makeSem ''BackupObjectStore

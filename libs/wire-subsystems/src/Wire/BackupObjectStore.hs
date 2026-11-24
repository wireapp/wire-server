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
import Imports
import Polysemy
import Wire.API.Backup

data PutObjectError
  = -- | The body was shorter than announced.
    PutIncompleteBody
  | -- | The upload took longer than allowed.
    PutTimedOut
  | PutFailed
  deriving (Eq, Show)

data BackupObjectStore m a where
  -- | Stream a body of the announced size into the object of the given file.
  PutObject ::
    BackupFileId ->
    Word64 ->
    ConduitT () ByteString (ResourceT IO) () ->
    BackupObjectStore m (Either PutObjectError ())

makeSem ''BackupObjectStore

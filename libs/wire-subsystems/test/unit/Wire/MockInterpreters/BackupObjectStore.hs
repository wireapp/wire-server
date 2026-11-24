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

module Wire.MockInterpreters.BackupObjectStore where

import Conduit (runConduitRes, sinkList, (.|))
import Data.ByteString qualified as BS
import Data.Map qualified as Map
import Data.Time.Clock (NominalDiffTime)
import Data.UUID qualified as UUID
import Imports
import Polysemy
import Polysemy.State
import URI.ByteString (parseURI, strictURIParserOptions)
import Wire.API.Backup
import Wire.BackupObjectStore
import Wire.MockInterpreters.Now (MockNow, passTime)

data PutBehaviour
  = PutSucceeds
  | PutFailsWith PutObjectError
  | -- | Succeeds, but only after the given time passed.
    PutTakes NominalDiffTime

inMemoryBackupObjectStore ::
  (Member (State (Map BackupFileId ByteString)) r, Member MockNow r, Member (Embed IO) r) =>
  PutBehaviour ->
  InterpreterFor BackupObjectStore r
inMemoryBackupObjectStore behaviour = interpret $ \case
  PutObject _ _ _ | PutFailsWith err <- behaviour -> pure (Left err)
  PutObject fid _ body -> do
    bytes <- embed (BS.concat <$> runConduitRes (body .| sinkList))
    modify (Map.insert fid bytes)
    case behaviour of
      PutTakes d -> passTime d
      _ -> pure ()
    pure (Right ())
  PresignGet _ (BackupFileId u) ->
    pure $ either (error . show) Imports.id $
      parseURI strictURIParserOptions ("https://s3.example/backups/" <> UUID.toASCIIBytes u)

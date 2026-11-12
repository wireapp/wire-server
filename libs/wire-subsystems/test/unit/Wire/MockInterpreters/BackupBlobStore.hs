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

module Wire.MockInterpreters.BackupBlobStore
  ( inMemoryBackupBlobStoreInterpreter,
  )
where

import Conduit (runConduitRes, sinkList, (.|))
import Data.ByteString qualified as BS
import Data.Map qualified as Map
import Data.Text.Encoding qualified as Text
import Data.UUID qualified as UUID
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.State
import URI.ByteString
import Wire.API.Backup
import Wire.BackupBlobStore

inMemoryBackupBlobStoreInterpreter ::
  ( Member (State (Map BackupFileId ByteString)) r,
    Member (Error BlobUploadError) r,
    Member (Embed IO) r
  ) =>
  InterpreterFor BackupBlobStore r
inMemoryBackupBlobStoreInterpreter = interpret $ \case
  PutBlob fid len src -> do
    body <- BS.concat <$> embed (runConduitRes (src .| sinkList))
    when (fromIntegral (BS.length body) /= len) $ throw BlobLengthMismatch
    modify (Map.insert fid body)
  SignedDownloadUrl fid ->
    either (error . show) pure
      . parseURI strictURIParserOptions
      . Text.encodeUtf8
      $ "https://blob.test/" <> UUID.toText (unBackupFileId fid)
  DeleteBlobs fids -> modify (\m -> foldr Map.delete m fids)

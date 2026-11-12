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
    countingBackupBlobStoreInterpreter,
    lockstepSource,
  )
where

import Conduit
import Control.Concurrent.STM (retry)
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
inMemoryBackupBlobStoreInterpreter = interpreter (const (pure ()))

-- | Like 'inMemoryBackupBlobStoreInterpreter', but adds the length of every consumed chunk to
-- the 'TVar' (see 'lockstepSource').
countingBackupBlobStoreInterpreter ::
  ( Member (State (Map BackupFileId ByteString)) r,
    Member (Error BlobUploadError) r,
    Member (Embed IO) r
  ) =>
  TVar Word64 ->
  InterpreterFor BackupBlobStore r
countingBackupBlobStoreInterpreter consumed =
  interpreter (atomically . modifyTVar' consumed . (+) . fromIntegral . BS.length)

interpreter ::
  ( Member (State (Map BackupFileId ByteString)) r,
    Member (Error BlobUploadError) r,
    Member (Embed IO) r
  ) =>
  (ByteString -> IO ()) ->
  InterpreterFor BackupBlobStore r
interpreter onChunk = interpret $ \case
  PutBlob fid len src -> do
    let counted = awaitForever (\c -> liftIO (onChunk c) >> yield c)
    body <- BS.concat <$> embed (runConduitRes (src .| counted .| sinkList))
    when (fromIntegral (BS.length body) /= len) $ throw BlobLengthMismatch
    modify (Map.insert fid body)
  SignedDownloadUrl fid ->
    either (error . show) pure
      . parseURI strictURIParserOptions
      . Text.encodeUtf8
      $ "https://blob.test/" <> UUID.toText (unBackupFileId fid)
  DeleteBlobs fids -> modify (\m -> foldr Map.delete m fids)

-- | @n@ chunks of @size@ bytes. Yields the next chunk only while at most 64 KiB produced bytes
-- are not yet consumed (per the 'TVar'), so it blocks forever if the consumer buffers the body.
lockstepSource :: TVar Word64 -> Int -> Int -> ConduitT () ByteString (ResourceT IO) ()
lockstepSource consumed n size = for_ [0 .. n - 1] $ \i -> do
  liftIO . atomically $ do
    c <- readTVar consumed
    unless (fromIntegral (i * size) - c <= 64 * 1024) retry
  yield (BS.replicate size (fromIntegral i))

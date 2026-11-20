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
  ( Blobs,
    Tombstones,
    inMemoryBackupBlobStoreInterpreter,
    countingBackupBlobStoreInterpreter,
    lockstepSource,
  )
where

import Conduit
import Control.Concurrent.STM (retry)
import Data.ByteString qualified as BS
import Data.Id (ClientId, UserId)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text.Encoding qualified as Text
import Data.UUID qualified as UUID
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.State
import URI.ByteString
import Wire.API.Backup
import Wire.BackupBlobStore
import Wire.BackupBlobStore.Key

-- | The stored objects. Deletes remove all versions, so one body per key suffices.
type Blobs = Map BackupLocation ByteString

-- | The tombstones of clients.
type Tombstones = Set (UserId, ClientId, BackupFileId)

inMemoryBackupBlobStoreInterpreter ::
  ( Member (State Blobs) r,
    Member (State Tombstones) r,
    Member (Error BlobUploadError) r,
    Member (Embed IO) r
  ) =>
  InterpreterFor BackupBlobStore r
inMemoryBackupBlobStoreInterpreter = interpreter (const (pure ()))

countingBackupBlobStoreInterpreter ::
  ( Member (State Blobs) r,
    Member (State Tombstones) r,
    Member (Error BlobUploadError) r,
    Member (Embed IO) r
  ) =>
  TVar Word64 ->
  InterpreterFor BackupBlobStore r
countingBackupBlobStoreInterpreter consumed =
  interpreter (atomically . modifyTVar' consumed . (+) . fromIntegral . BS.length)

interpreter ::
  ( Member (State Blobs) r,
    Member (State Tombstones) r,
    Member (Error BlobUploadError) r,
    Member (Embed IO) r
  ) =>
  (ByteString -> IO ()) ->
  InterpreterFor BackupBlobStore r
interpreter onChunk = interpret $ \case
  PutBlob l len src -> do
    let counted = awaitForever (\c -> liftIO (onChunk c) >> yield c)
    body <- BS.concat <$> embed (runConduitRes (src .| counted .| sinkList))
    -- Stored before the mismatch is reported: the S3 object may exist then.
    modify (Map.insert l body)
    when (fromIntegral (BS.length body) /= len) $ throw BlobLengthMismatch
  BlobExists l -> gets (Map.member l)
  SignedDownloadUrl l ->
    either (error . show) pure
      . parseURI strictURIParserOptions
      . Text.encodeUtf8
      $ "https://blob.test/" <> UUID.toText (unBackupFileId (fileRefId l.file))
  ListBlobEpochs u c -> do
    files <- gets Map.keys
    pure
      . Set.toDescList
      . Set.fromList
      . map (.epoch)
      $ filter (\l -> l.user == u && l.client == c) files
  ListBlobs u c e ->
    gets $
      reverse
        . map (\(l, b) -> BackupBlob {file = l.file, size = fromIntegral (BS.length b)})
        . filter (\(l, _) -> l.user == u && l.client == c && l.epoch == e)
        . Map.toList
  DeleteBlob l -> modify (Map.delete l)
  DeleteEpochFiles u c e fs ->
    modify (\m -> foldr (\f -> Map.delete BackupLocation {user = u, client = c, epoch = e, file = f}) m fs)
  DeleteBlobs p -> do
    modify (Map.filterWithKey (\l _ -> not (inPrefix p l)))
    modify (Set.filter (\(u, c, _) -> not (tombstoneInPrefix p u c)))
  PutTombstone u c i -> modify (Set.insert (u, c, i))
  ListTombstones u c -> gets (map (\(_, _, i) -> i) . filter (\(u', c', _) -> (u', c') == (u, c)) . Set.toList)
  DeleteClientBackupsBefore u c t -> do
    modify (Map.filterWithKey (\l _ -> not (inPrefix (ClientPrefix u c) l && fullFileId l.file < t)))
    modify (Set.filter (\(u', c', i) -> (u', c') /= (u, c) || i >= t))

-- | Tombstones are below the client prefix, not below an epoch prefix.
tombstoneInPrefix :: BackupPrefix -> UserId -> ClientId -> Bool
tombstoneInPrefix p u c = case p of
  UserPrefix u' -> u == u'
  ClientPrefix u' c' -> u == u' && c == c'
  EpochPrefix {} -> False

inPrefix :: BackupPrefix -> BackupLocation -> Bool
inPrefix p l = case p of
  UserPrefix u -> l.user == u
  ClientPrefix u c -> l.user == u && l.client == c
  EpochPrefix u c e -> l.user == u && l.client == c && l.epoch == e

lockstepSource :: TVar Word64 -> Int -> Int -> ConduitT () ByteString (ResourceT IO) ()
lockstepSource consumed n size = for_ [0 .. n - 1] $ \i -> do
  liftIO . atomically $ do
    c <- readTVar consumed
    unless (fromIntegral (i * size) - c <= 64 * 1024) retry
  yield (BS.replicate size (fromIntegral i))

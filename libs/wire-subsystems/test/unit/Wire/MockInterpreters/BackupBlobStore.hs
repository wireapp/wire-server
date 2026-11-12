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
  ( BlobVersions,
    versionsOf,
    inMemoryBackupBlobStoreInterpreter,
    countingBackupBlobStoreInterpreter,
    lockstepSource,
  )
where

import Conduit
import Control.Concurrent.STM (retry)
import Data.ByteString qualified as BS
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
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

-- | Models a versioned bucket: version history per key, newest first, each with its
-- version id. 'Nothing' is a delete marker.
type BlobVersions = Map BackupLocation [(Int, Maybe ByteString)]

-- | All versions of a key (including delete markers), newest first.
versionsOf :: BackupLocation -> BlobVersions -> [Maybe ByteString]
versionsOf l = map snd . Map.findWithDefault [] l

inMemoryBackupBlobStoreInterpreter ::
  ( Member (State BlobVersions) r,
    Member (Error BlobUploadError) r,
    Member (Embed IO) r
  ) =>
  InterpreterFor BackupBlobStore r
inMemoryBackupBlobStoreInterpreter = interpreter (const (pure ()))

countingBackupBlobStoreInterpreter ::
  ( Member (State BlobVersions) r,
    Member (Error BlobUploadError) r,
    Member (Embed IO) r
  ) =>
  TVar Word64 ->
  InterpreterFor BackupBlobStore r
countingBackupBlobStoreInterpreter consumed =
  interpreter (atomically . modifyTVar' consumed . (+) . fromIntegral . BS.length)

interpreter ::
  ( Member (State BlobVersions) r,
    Member (Error BlobUploadError) r,
    Member (Embed IO) r
  ) =>
  (ByteString -> IO ()) ->
  InterpreterFor BackupBlobStore r
interpreter onChunk = interpret $ \case
  PutBlob l len src -> do
    let counted = awaitForever (\c -> liftIO (onChunk c) >> yield c)
    body <- BS.concat <$> embed (runConduitRes (src .| counted .| sinkList))
    when (fromIntegral (BS.length body) /= len) $ throw BlobLengthMismatch
    Just . Text.pack . show <$> addVersion l (Just body)
  BlobExists l -> gets (Map.member l . live)
  SignedDownloadUrl l ->
    either (error . show) pure
      . parseURI strictURIParserOptions
      . Text.encodeUtf8
      $ "https://blob.test/" <> UUID.toText (unBackupFileId l.fileId)
  ListBlobEpochs u c ->
    gets $ Set.toDescList . Set.fromList . map (.epoch) . filter (\l -> l.user == u && l.client == c) . Map.keys . live
  ListBlobs u c e ->
    gets $
      reverse
        . map (\(l, b) -> BackupBlob {kind = l.kind, fileId = l.fileId, size = fromIntegral (BS.length b)})
        . filter (\(l, _) -> l.user == u && l.client == c && l.epoch == e)
        . Map.toList
        . live
  DeleteBlobVersion l Nothing -> void (addVersion l Nothing)
  DeleteBlobVersion l (Just v) ->
    modify $ Map.update (nonEmpty' . filter (\(i, _) -> Just i /= readMaybe (Text.unpack v))) l
  DeleteBlob l -> modify (Map.delete l)
  DeleteBlobs p -> modify (Map.filterWithKey (\l _ -> not (inPrefix p l)))
  where
    addVersion l v = do
      h <- gets (Map.findWithDefault [] l)
      let i = 1 + maximum (0 : map fst h)
      modify (Map.insert l ((i, v) : h))
      pure i

    nonEmpty' h = if null h then Nothing else Just h

    live :: BlobVersions -> Map BackupLocation ByteString
    live = Map.mapMaybe (\case (_, Just b) : _ -> Just b; _ -> Nothing)

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

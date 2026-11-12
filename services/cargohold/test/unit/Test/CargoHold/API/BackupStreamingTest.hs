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

-- | The real HTTP stack (warp, servant, 'backupServer') must stream backup uploads.
module Test.CargoHold.API.BackupStreamingTest (tests) where

import CargoHold.API.Backup (backupServer)
import Conduit
import Control.Concurrent.STM (check)
import qualified Data.ByteString as BS
import Data.ByteString.Conversion (toByteString')
import Data.Domain (Domain (..))
import Data.Id
import Data.Json.Util (toUTCTimeMillis)
import Data.Proxy
import Data.Time (UTCTime (..), fromGregorian)
import qualified Data.UUID as UUID
import Imports
import qualified Network.HTTP.Client as Http
import Network.HTTP.Types (hContentType, methodPost, statusCode)
import Network.Wai.Handler.Warp (testWithApplication)
import Polysemy
import Servant (Context (..), serveWithContext)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Backup
import Wire.API.Routes.API (hoistServerWithDomain)
import Wire.API.Routes.Public.Cargohold.Backup (BackupAPI)
import Wire.API.Routes.Version.Wai (versionMiddleware)
import Wire.BackupSubsystem

tests :: TestTree
tests =
  testGroup
    "BackupStreaming"
    [ testCase "backup upload streams through HTTP stack" uploadStreams,
      testCase "backup upload without Content-Length is rejected" uploadChunked
    ]

chunkCount, chunkSize :: Int
chunkCount = 64
chunkSize = 16 * 1024

uploadStreams :: IO ()
uploadStreams = do
  received <- newTVarIO 0
  stored <- newIORef Nothing
  popper <- lockstepPopper received
  let body = Http.RequestBodyStream (fromIntegral (chunkCount * chunkSize)) ($ popper)
  status <- timeout (10 * 1000 * 1000) (postUpload received stored body)
  status @?= Just 200
  readIORef stored >>= (@?= Just (foldMap chunk [0 .. chunkCount - 1]))

-- | Without @Content-Length@ servant rejects the request before the subsystem is called.
uploadChunked :: IO ()
uploadChunked = do
  received <- newTVarIO 0
  stored <- newIORef Nothing
  sent <- newIORef False
  let popper = bool (chunk 0) mempty <$> atomicModifyIORef' sent (True,)
  status <-
    timeout (10 * 1000 * 1000) $
      postUpload received stored (Http.RequestBodyStreamChunked ($ popper))
  status @?= Just 400
  readIORef stored >>= (@?= Nothing)

-- | POSTs @body@ to the upload route served by 'backupServer' in warp, returns the status.
postUpload :: TVar Word64 -> IORef (Maybe ByteString) -> Http.RequestBody -> IO Int
postUpload received stored body = do
  let app =
        versionMiddleware mempty $
          serveWithContext
            (Proxy @BackupAPI)
            (Domain "example.com" :. EmptyContext)
            ( hoistServerWithDomain @BackupAPI
                (liftIO . runM . fakeUploads received stored)
                backupServer
            )
      cid = ClientId 1
  testWithApplication (pure app) $ \port -> do
    mgr <- Http.newManager Http.defaultManagerSettings
    let req =
          Http.defaultRequest
            { Http.port = port,
              Http.method = methodPost,
              Http.path = "/v19/backups/clients/" <> toByteString' cid <> "/epochs/1",
              Http.requestHeaders =
                [ ("Z-User", toByteString' (Id @() UUID.nil)),
                  ("Z-Client", toByteString' cid),
                  (hContentType, "application/octet-stream")
                ],
              Http.requestBody = body
            }
    statusCode . Http.responseStatus <$> Http.httpNoBody req mgr

chunk :: Int -> ByteString
chunk i = BS.replicate chunkSize (fromIntegral i)

-- | Returns the next chunk only while at most 64 KiB sent bytes are not yet received by the
-- blob store, so it blocks forever (until the timeout) if any layer buffers the body.
lockstepPopper :: TVar Word64 -> IO (IO ByteString)
lockstepPopper received = do
  next <- newIORef 0
  pure $ do
    i <- readIORef next
    if i >= chunkCount
      then pure mempty
      else do
        atomically $ do
          r <- readTVar received
          check (toInteger (i * chunkSize) - toInteger r <= 64 * 1024)
        writeIORef next (i + 1)
        pure (chunk i)

-- | Only uploads are supported: consumes the stream like a blob store would.
fakeUploads ::
  (Member (Embed IO) r) =>
  TVar Word64 ->
  IORef (Maybe ByteString) ->
  InterpreterFor BackupSubsystem r
fakeUploads received stored = interpret $ \case
  UploadBackup _ _ _ _ len src -> embed $ do
    let count = awaitForever $ \c -> do
          liftIO . atomically $ modifyTVar' received (+ fromIntegral (BS.length c))
          yield c
    body <- runConduitRes (src .| count .| foldC)
    writeIORef stored (Just body)
    pure
      BackupFile
        { id = BackupFileId UUID.nil,
          kind = BackupFull,
          size = len,
          createdAt = toUTCTimeMillis (UTCTime (fromGregorian 2026 1 1) 0)
        }
  _ -> error "fakeUploads: unsupported operation"

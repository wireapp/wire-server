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
import Data.Default (def)
import Data.Domain (Domain (..))
import Data.Id
import qualified Data.Map as Map
import Data.Proxy
import qualified Data.Set as Set
import Data.Typeable (cast)
import Imports
import qualified Network.HTTP.Client as Http
import Network.HTTP.Types (hContentType, methodPost, statusCode)
import Network.Wai.Handler.Warp (testWithApplication)
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Servant (Context (..), serveWithContext)
import System.Timeout (timeout)
import Test.QuickCheck (arbitrary, generate)
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Routes.API (hoistServerWithDomain)
import Wire.API.Routes.Public.Cargohold.Backup (BackupAPI)
import Wire.API.Routes.Version.Wai (versionMiddleware)
import Wire.API.Team.Feature (AutomaticBackupsConfig, FeatureStatus (..), LockableFeature (..))
import Wire.API.User (User (..), userId)
import Wire.API.User.Client (UserClients (..))
import Wire.BackupBlobStore
import Wire.BackupSubsystem
import Wire.BackupSubsystem.Interpreter (interpretBackupSubsystem)
import Wire.BackupSubsystem.Rules (BackupLimits (..))
import Wire.BrigAPIAccess
import Wire.GalleyAPIAccess
import Wire.Sem.Now (Now)
import Wire.Sem.Now.IO (nowToIO)
import Wire.Sem.Random (Random)
import Wire.Sem.Random.IO (randomToIO)

tests :: TestTree
tests =
  testGroup
    "BackupStreaming"
    [ testCase "backup upload streams through HTTP stack" uploadStreams,
      testCase "backup upload without Content-Length is rejected" uploadChunked
    ]

cid :: ClientId
cid = ClientId 1

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
  base <- generate arbitrary
  tid <- generate arbitrary
  let user = base {userTeam = Just tid}
      app =
        versionMiddleware mempty $
          serveWithContext
            (Proxy @BackupAPI)
            (Domain "example.com" :. EmptyContext)
            ( hoistServerWithDomain @BackupAPI
                (liftIO . runSubsystem user received stored)
                backupServer
            )
  testWithApplication (pure app) $ \port -> do
    mgr <- Http.newManager Http.defaultManagerSettings
    let req =
          Http.defaultRequest
            { Http.port = port,
              Http.method = methodPost,
              Http.path = "/v19/backups/clients/" <> toByteString' cid <> "/epochs/1",
              Http.requestHeaders =
                [ ("Z-User", toByteString' (userId user)),
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

-- | The real 'interpretBackupSubsystem'; brig, galley and the blob store are minimal fakes for one
-- user with client 'cid' in a team with the feature enabled.
runSubsystem ::
  User ->
  TVar Word64 ->
  IORef (Maybe ByteString) ->
  Sem
    '[ BackupSubsystem,
       BackupBlobStore,
       BrigAPIAccess,
       GalleyAPIAccess,
       Random,
       Now,
       Input BackupLimits,
       Error BackupError,
       Error BlobUploadError,
       Embed IO
     ]
    a ->
  IO a
runSubsystem user received stored =
  fmap (either (error . show) Imports.id)
    . runM
    . runError @BlobUploadError
    . fmap (either (error . show) Imports.id)
    . runError @BackupError
    . runInputConst limits
    . nowToIO
    . randomToIO
    . fakeGalley
    . fakeBrig user
    . countingBlobStore received stored
    . interpretBackupSubsystem

limits :: BackupLimits
limits =
  BackupLimits
    { maxFullBytes = 2 ^ (30 :: Int),
      maxIncrementalBytes = 0,
      maxFilesPerEpoch = 1
    }

-- | Simplified copy of @Wire.MockInterpreters.BackupBlobStore@ (wire-subsystems tests): an
-- empty store holding at most one blob; adds the length of every consumed chunk to the 'TVar'.
countingBlobStore ::
  (Member (Embed IO) r, Member (Error BlobUploadError) r) =>
  TVar Word64 ->
  IORef (Maybe ByteString) ->
  InterpreterFor BackupBlobStore r
countingBlobStore received stored = interpret $ \case
  PutBlob _ len src -> do
    let count = awaitForever $ \c -> do
          liftIO . atomically $ modifyTVar' received (+ fromIntegral (BS.length c))
          yield c
    body <- embed $ runConduitRes (src .| count .| foldC)
    when (fromIntegral (BS.length body) /= len) $ throw BlobLengthMismatch
    writeIORef stored (Just body)
    pure Nothing
  ListBlobEpochs _ _ -> pure []
  ListBlobs {} -> pure []
  DeleteBlobVersion _ _ -> writeIORef stored Nothing
  DeleteBlob _ -> writeIORef stored Nothing
  DeleteBlobs _ -> writeIORef stored Nothing
  _ -> error "countingBlobStore: unexpected call"

fakeBrig :: User -> InterpreterFor BrigAPIAccess r
fakeBrig user = interpret $ \case
  GetUsers _ -> pure [user]
  LookupClients _ -> pure . UserClients $ Map.singleton (userId user) (Set.singleton cid)
  _ -> error "fakeBrig: unexpected call"

fakeGalley :: InterpreterFor GalleyAPIAccess r
fakeGalley = interpret $ \case
  GetFeatureConfigForTeam _ -> do
    let feat :: LockableFeature AutomaticBackupsConfig = def {status = FeatureStatusEnabled}
    pure . fromMaybe (error "fakeGalley: unexpected feature") $ cast feat
  _ -> error "fakeGalley: unexpected call"

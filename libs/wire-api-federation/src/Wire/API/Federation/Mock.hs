{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

-- | GRPC Server Mocking Machinery
module Wire.API.Federation.Mock where

import qualified Control.Concurrent.Async as Async
import Control.Exception.Lifted (finally)
import Control.Monad.Except (ExceptT (..), MonadError (..), runExceptT)
import Control.Monad.State (MonadState (..), modify)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Domain (Domain)
import Data.EitherR
import qualified Data.Text as Text
import Imports
import Mu.GRpc.Client.Record (grpcClientConfigSimple)
import Mu.GRpc.Server (gRpcAppTrans, msgProtoBuf)
import Mu.Server (ServerError, ServerErrorIO, SingleServerT)
import qualified Mu.Server as Mu
import qualified Network.Wai.Handler.Warp as Warp
import System.Timeout (timeout)
import Wire.API.Federation.Client (FederatorClient, runFederatorClientWith)
import Wire.API.Federation.GRPC.Client (createGrpcClient, reason)
import Wire.API.Federation.GRPC.Types (FederatedRequest, Outward, OutwardError, OutwardResponse (OutwardResponseBody, OutwardResponseError))

type ReceivedRequests = [FederatedRequest]

-- FUTUREWORK: check if target domain matches the one in the request
outwardService :: SingleServerT info Outward (MockT ServerErrorIO) '[ '[FederatedRequest -> MockT ServerErrorIO OutwardResponse]]
outwardService = Mu.singleService (Mu.method @"call" callOutward)

callOutward :: FederatedRequest -> MockT ServerErrorIO OutwardResponse
callOutward req = do
  modify (\s -> s {receivedRequests = receivedRequests s <> [req]})
  MockT . lift . effectfulResponse =<< get

mkSuccessResponse :: Aeson.ToJSON a => a -> ServerErrorIO OutwardResponse
mkSuccessResponse = pure . OutwardResponseBody . LBS.toStrict . Aeson.encode

mkErrorResponse :: OutwardError -> ServerErrorIO OutwardResponse
mkErrorResponse = pure . OutwardResponseError

startMockFederator :: IORef MockState -> ExceptT String IO ()
startMockFederator ref = ExceptT $ do
  (port, sock) <- Warp.openFreePort
  serverStarted <- newEmptyMVar
  let settings =
        Warp.defaultSettings
          & Warp.setPort port
          & Warp.setGracefulCloseTimeout2 0 -- Defaults to 2 seconds, causes server stop to take very long
          & Warp.setBeforeMainLoop (putMVar serverStarted ())
  let app = gRpcAppTrans msgProtoBuf (runMockT ref) outwardService
  federatorThread <- Async.async $ Warp.runSettingsSocket settings sock app
  serverStartedSignal <- timeout 10_000_000 (takeMVar serverStarted)
  case serverStartedSignal of
    Nothing -> do
      liftIO $ Async.cancel federatorThread
      pure . Left $ "Failed to start the mock server within 10 seconds on port: " <> show port
    _ -> do
      liftIO . modifyIORef ref $ \s -> s {serverThread = federatorThread, serverPort = toInteger port}
      pure (Right ())

stopMockFederator :: IORef MockState -> IO ()
stopMockFederator ref = do
  Async.cancel . serverThread <=< readIORef $ ref

flushState :: IORef MockState -> IO ()
flushState = flip modifyIORef $ \s -> s {receivedRequests = [], effectfulResponse = error "No mock response provided"}

initState :: Domain -> Domain -> MockState
initState targetDomain originDomain = MockState [] (error "No mock response provided") (error "server not started") (error "No port selected yet") targetDomain originDomain

withMockFederator ::
  IORef MockState ->
  ServerErrorIO OutwardResponse ->
  (MockState -> ExceptT String IO a) ->
  ExceptT String IO (a, ReceivedRequests)
withMockFederator ref resp action = do
  liftIO . modifyIORef ref $ \s -> s {effectfulResponse = resp}
  st <- liftIO $ readIORef ref
  actualResponse <- action st
  st' <- liftIO $ readIORef ref
  pure (actualResponse, receivedRequests st')

withMockFederatorClient ::
  IORef MockState ->
  ServerErrorIO OutwardResponse ->
  FederatorClient component (ExceptT e IO) a ->
  ExceptT String IO (Either e a, ReceivedRequests)
withMockFederatorClient ref resp action = withMockFederator ref resp $ \st -> do
  let cfg = grpcClientConfigSimple "127.0.0.1" (fromInteger (serverPort st)) False
  client <- fmapLT (Text.unpack . reason) (ExceptT (createGrpcClient cfg))
  lift . runExceptT $
    runFederatorClientWith client (stateTarget st) (stateOrigin st) action

-- | Like 'withMockFederator', but spawn a new instance of the mock federator
-- just for this action.
withTempMockFederator ::
  forall a.
  MockState ->
  ServerErrorIO OutwardResponse ->
  (MockState -> ExceptT String IO a) ->
  ExceptT String IO (a, ReceivedRequests)
withTempMockFederator st resp action = do
  ref <- liftIO $ newIORef st
  startMockFederator ref
  withMockFederator ref resp action
    `finally` lift (stopMockFederator ref)

newtype MockT m a = MockT {unMock :: ReaderT (IORef MockState) m a}
  deriving newtype (Functor, Applicative, Monad, MonadReader (IORef MockState), MonadIO)

instance (MonadError ServerError m) => MonadError ServerError (MockT m) where
  throwError err = MockT . lift $ throwError err
  catchError action f = do
    r <- ask
    MockT . lift $ catchError (runMockT r action) (runMockT r . f)

instance MonadIO m => MonadState MockState (MockT m) where
  get = readIORef =<< ask
  put x = do
    ref <- ask
    writeIORef ref x

data MockState = MockState
  { receivedRequests :: ReceivedRequests,
    effectfulResponse :: ServerErrorIO OutwardResponse,
    serverThread :: Async.Async (),
    serverPort :: Integer,
    stateTarget :: Domain,
    stateOrigin :: Domain
  }

runMockT :: IORef MockState -> MockT m a -> m a
runMockT ref mock = runReaderT (unMock mock) ref

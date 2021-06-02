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
import Control.Monad.Catch (MonadMask)
import qualified Control.Monad.Catch as Catch
import Control.Monad.Except (ExceptT (..), MonadError (..), runExceptT)
import Control.Monad.State (MonadState (..), modify, gets)
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
  resp <- gets effectfulResponse
  MockT . lift $ resp req

mkSuccessResponse :: Aeson.ToJSON a => a -> ServerErrorIO OutwardResponse
mkSuccessResponse = pure . OutwardResponseBody . LBS.toStrict . Aeson.encode

mkErrorResponse :: OutwardError -> ServerErrorIO OutwardResponse
mkErrorResponse = pure . OutwardResponseError

startMockFederator :: MonadIO m => IORef MockState -> ExceptT String m ()
startMockFederator ref = ExceptT . liftIO $ do
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

stopMockFederator :: MonadIO m => IORef MockState -> m ()
stopMockFederator ref =
  liftIO $ Async.cancel . serverThread <=< readIORef $ ref

flushState :: IORef MockState -> IO ()
flushState = flip modifyIORef $ \s -> s {receivedRequests = [], effectfulResponse = error "No mock response provided"}

initState :: Domain -> Domain -> MockState
initState = MockState [] (error "No mock response provided") (error "server not started") (error "No port selected yet")

-- | Run an action with access to a mock federator.
--
-- The function argument `resp :: FederatedRequest -> ServerErrorIO
-- OutwardResponse` can be used to provide a fake federator response for each
-- possible request it is expected to receive.
--
-- More explicitly, any request `req` to the federator within the provided
-- action will return `resp req` as its response.
withMockFederator ::
  (MonadIO m, MonadMask m) =>
  IORef MockState ->
  (FederatedRequest -> ServerErrorIO OutwardResponse) ->
  (MockState -> ExceptT String m a) ->
  ExceptT String m (a, ReceivedRequests)
withMockFederator ref resp action = do
  liftIO . modifyIORef ref $ \s -> s {effectfulResponse = resp}
  st <- liftIO $ readIORef ref
  actualResponse <- action st
  st' <- liftIO $ readIORef ref
  pure (actualResponse, receivedRequests st')

withMockFederatorClient ::
  (MonadIO m, MonadMask m) =>
  IORef MockState ->
  (FederatedRequest -> ServerErrorIO OutwardResponse) ->
  FederatorClient component (ExceptT e m) a ->
  ExceptT String m (Either e a, ReceivedRequests)
withMockFederatorClient ref resp action = withMockFederator ref resp $ \st -> do
  let cfg = grpcClientConfigSimple "127.0.0.1" (fromInteger (serverPort st)) False
  client <- fmapLT (Text.unpack . reason) (ExceptT (createGrpcClient cfg))
  lift . runExceptT $
    runFederatorClientWith client (stateTarget st) (stateOrigin st) action

-- | Like 'withMockFederator', but spawn a new instance of the mock federator
-- just for this action.
withTempMockFederator ::
  (MonadIO m, MonadMask m) =>
  MockState ->
  (FederatedRequest -> ServerErrorIO OutwardResponse) ->
  (MockState -> ExceptT String m a) ->
  ExceptT String m (a, ReceivedRequests)
withTempMockFederator st resp action = do
  ref <- newIORef st
  startMockFederator ref
  withMockFederator ref resp action
    `Catch.finally` stopMockFederator ref

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
    effectfulResponse :: FederatedRequest -> ServerErrorIO OutwardResponse,
    serverThread :: Async.Async (),
    serverPort :: Integer,
    stateTarget :: Domain,
    stateOrigin :: Domain
  }

runMockT :: IORef MockState -> MockT m a -> m a
runMockT ref mock = runReaderT (unMock mock) ref

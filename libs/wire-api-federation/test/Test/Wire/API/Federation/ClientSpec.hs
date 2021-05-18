{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Wire.API.Federation.ClientSpec where

import qualified Control.Concurrent.Async as Async
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.State (MonadState (..), modify)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Domain (Domain (Domain))
import qualified Data.Text as Text
import Imports
import Mu.GRpc.Client.Record (grpcClientConfigSimple)
import Mu.GRpc.Server (gRpcAppTrans, msgProtoBuf)
import Mu.Server (ServerError, ServerErrorIO, SingleServerT)
import qualified Mu.Server as Mu
import qualified Network.Wai.Handler.Warp as Warp
import System.Timeout (timeout)
import Test.Hspec
import Test.QuickCheck (arbitrary, generate)
import qualified Wire.API.Federation.API.Brig as Brig
import Wire.API.Federation.Client (FederationClientError (FederationClientOutwardError, FederationClientRPCError), FederatorClient, runFederatorClientWith)
import Wire.API.Federation.GRPC.Client (createGrpcClient)
import Wire.API.Federation.GRPC.Types (Component (Brig), FederatedRequest (FederatedRequest), Outward, OutwardError, OutwardResponse (OutwardResponseBody, OutwardResponseError), Request (..))
import Wire.API.User (UserProfile)

spec :: Spec
spec = do
  stateRef <- runIO initState
  beforeAll (startMockFederator stateRef)
    . afterAll_ (stopMockFederator stateRef)
    . before_ (flushState stateRef)
    $ describe "Federator.Client" $ do
      it "should make correct calls to the federator and parse success response correctly" $ do
        handle <- generate arbitrary
        expectedResponse :: Maybe UserProfile <- generate arbitrary

        (actualResponse, sentRequests) <-
          withMockFederator stateRef (mkSuccessResponse expectedResponse) $
            Brig.getUserByHandle Brig.clientRoutes handle

        sentRequests `shouldBe` [FederatedRequest "target.example.com" (Just $ Request Brig "/federation/get-user-by-handle" (LBS.toStrict (Aeson.encode handle)) "origin.example.com")]
        actualResponse `shouldBe` Right expectedResponse

      it "should parse failure response correctly" $ do
        handle <- generate arbitrary
        someErr <- generate arbitrary

        (actualResponse, _) <-
          withMockFederator stateRef (mkErrorResponse someErr) $
            Brig.getUserByHandle Brig.clientRoutes handle

        actualResponse `shouldBe` Left (FederationClientOutwardError someErr)

      it "should report federator failures correctly" $ do
        handle <- generate arbitrary

        (actualResponse, _) <-
          withMockFederator stateRef (error "some IO error!") $
            Brig.getUserByHandle Brig.clientRoutes handle

        case actualResponse of
          Right res ->
            expectationFailure $ "Expected response to be failure, got: \n" <> show res
          Left (FederationClientRPCError errText) ->
            Text.unpack errText `shouldStartWith` "grpc error: GRPC status indicates failure: status-code=INTERNAL, status-message=\"some IO error!"
          Left err ->
            expectationFailure $ "Expected FedeartionClientRPCError, got different error: \n" <> show err

      it "should report GRPC errors correctly" $ do
        handle <- generate arbitrary

        (actualResponse, _) <-
          withMockFederator stateRef (throwError $ Mu.ServerError Mu.NotFound "Just testing") $
            Brig.getUserByHandle Brig.clientRoutes handle

        actualResponse `shouldBe` Left (FederationClientRPCError "grpc error: GRPC status indicates failure: status-code=NOT_FOUND, status-message=\"Just testing\"")

-- * GRPC Server Mocking Machinery

type RecievedRequests = [FederatedRequest]

outwardService :: SingleServerT info Outward (MockT ServerErrorIO) _
outwardService = Mu.singleService (Mu.method @"call" callOutward)

callOutward :: FederatedRequest -> MockT ServerErrorIO OutwardResponse
callOutward req = do
  modify (\s -> s {recievedRequests = recievedRequests s <> [req]})
  MockT . lift . effectfulResponse =<< get

mkSuccessResponse :: Aeson.ToJSON a => a -> ServerErrorIO OutwardResponse
mkSuccessResponse = pure . OutwardResponseBody . LBS.toStrict . Aeson.encode

mkErrorResponse :: OutwardError -> ServerErrorIO OutwardResponse
mkErrorResponse = pure . OutwardResponseError

startMockFederator :: IORef MockState -> IO ()
startMockFederator ref = do
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
      expectationFailure $ "Failed to start the mock server within 10 seconds on port: " <> show port
      Async.cancel federatorThread
    _ -> pure ()
  modifyIORef ref $ \s -> s {serverThread = federatorThread, serverPort = toInteger port}

stopMockFederator :: IORef MockState -> IO ()
stopMockFederator ref = do
  Async.cancel . serverThread <=< readIORef $ ref

flushState :: IORef MockState -> IO ()
flushState = flip modifyIORef $ \s -> s {recievedRequests = [], effectfulResponse = error "No mock response provided"}

initState :: IO (IORef MockState)
initState = newIORef $ MockState [] (error "No mock response provided") (error "server not started") (error "No port selected yet")

-- This is mostly copy-pasta from brig-integration. Perhaps this should be part
-- of the wire-api-federation libarary?
withMockFederator :: IORef MockState -> ServerErrorIO OutwardResponse -> FederatorClient 'Brig (ExceptT FederationClientError IO) a -> IO (Either FederationClientError a, RecievedRequests)
withMockFederator ref res action = do
  modifyIORef ref $ \s -> s {effectfulResponse = res}
  port <- serverPort <$> readIORef ref
  let cfg = grpcClientConfigSimple "127.0.0.1" (fromInteger port) False
  client <- assertRight =<< createGrpcClient cfg
  actualResponse <- runExceptT (runFederatorClientWith client targetDomain originDomain action)

  reqs <- readIORef ref
  pure (actualResponse, recievedRequests reqs)
  where
    targetDomain = Domain "target.example.com"
    originDomain = Domain "origin.example.com"

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
  { recievedRequests :: RecievedRequests,
    effectfulResponse :: ServerErrorIO OutwardResponse,
    serverThread :: Async.Async (),
    serverPort :: Integer
  }

runMockT :: IORef MockState -> MockT m a -> m a
runMockT ref mock = runReaderT (unMock mock) ref

assertRight :: Show a => Either a b -> IO b
assertRight = \case
  Left a -> do
    expectationFailure $ "Expected Right, got Left: " <> show a
    error "impossible"
  Right b -> pure b

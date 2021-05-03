{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
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
import Control.Exception (bracket, finally, throwIO, try)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.State (MonadState (..), modify)
import Control.Retry (constantDelay, limitRetries, retrying)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Domain (Domain (Domain))
import qualified Data.Text as Text
import Foreign.C (Errno (Errno), eCONNREFUSED)
import GHC.IO.Exception (ioe_errno)
import Imports
import Mu.GRpc.Client.Record (grpcClientConfigSimple)
import Mu.GRpc.Server (msgProtoBuf, runGRpcAppTrans)
import Mu.Server (ServerError, ServerErrorIO, SingleServerT)
import qualified Mu.Server as Mu
import Network.Socket (Family (AF_INET), SockAddr (SockAddrInet), SocketType (Stream), close', connect, socket, tupleToHostAddress)
import Test.Hspec
import Test.QuickCheck (arbitrary, generate)
import qualified Wire.API.Federation.API.Brig as Brig
import Wire.API.Federation.Client (FederationClientError (FederationClientOutwardError, FederationClientRPCError), FederatorClient, runFederatorClientWith)
import Wire.API.Federation.GRPC.Client (createGrpcClient)
import Wire.API.Federation.GRPC.Types (Component (Brig), FederatedRequest (FederatedRequest), Outward, OutwardError, OutwardResponse (OutwardResponseBody, OutwardResponseError), Request (..))
import Wire.API.User (UserProfile)

spec :: Spec
spec = fdescribe "Federator.Client" $ do
  it "should make correct calls to the federator and parse success response correctly" $ do
    handle <- generate arbitrary
    expectedResponse :: Maybe UserProfile <- generate arbitrary

    (actualResponse, sentRequests) <-
      withMockFederator (mkSuccessResponse expectedResponse) $
        Brig.getUserByHandle Brig.clientRoutes handle

    sentRequests `shouldBe` [FederatedRequest "target.example.com" (Just $ Request Brig "/federation/users/by-handle" (LBS.toStrict (Aeson.encode handle)) "origin.example.com")]
    actualResponse `shouldBe` Right expectedResponse

  it "should parse failure response correctly" $ do
    handle <- generate arbitrary
    someErr <- generate arbitrary

    (actualResponse, _) <-
      withMockFederator (mkErrorResponse someErr) $
        Brig.getUserByHandle Brig.clientRoutes handle

    actualResponse `shouldBe` Left (FederationClientOutwardError someErr)

  it "should report federator failures correctly" $ do
    handle <- generate arbitrary

    (actualResponse, _) <-
      withMockFederator (error "some IO error!") $
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
      withMockFederator (throwError $ Mu.ServerError Mu.NotFound "Just testing") $
        Brig.getUserByHandle Brig.clientRoutes handle

    actualResponse `shouldBe` FederationClientRPCError "grpc error: GRPC status indicates failure: status-code=NOT_FOUND, status-message=\"Just testing\""

-- * GRPC Server Mocking Machinery

type RecievedRequests = [FederatedRequest]

outwardService :: ServerErrorIO OutwardResponse -> SingleServerT info Outward (MockT ServerErrorIO) _
outwardService res = Mu.singleService (Mu.method @"call" (callOutward res))

callOutward :: ServerErrorIO OutwardResponse -> FederatedRequest -> MockT ServerErrorIO OutwardResponse
callOutward res req = do
  modify (<> [req])
  MockT . lift $ res

mkSuccessResponse :: Aeson.ToJSON a => a -> ServerErrorIO OutwardResponse
mkSuccessResponse = pure . OutwardResponseBody . LBS.toStrict . Aeson.encode

mkErrorResponse :: OutwardError -> ServerErrorIO OutwardResponse
mkErrorResponse = pure . OutwardResponseError

-- This is mostly copy-pasta from brig-integration. Perhaps this should be part
-- of the wire-api-federation libarary?
withMockFederator :: ServerErrorIO OutwardResponse -> FederatorClient 'Brig (ExceptT FederationClientError IO) a -> IO (Either FederationClientError a, RecievedRequests)
withMockFederator res action = do
  let port = 47282 -- TODO: Make this actually arbitrary?
  requestsRef <- newIORef []
  federatorThread <- Async.async $ runGRpcAppTrans msgProtoBuf port (runMockT requestsRef) (outwardService res)
  -- FUTUREWORK: This takes exactly 2 seconds, perhaps something sleeps when grpc server starts, investigate
  serverStarted <- retryWhileN 5 not (isPortOpen port)
  serverStarted `shouldBe` True

  let cfg = grpcClientConfigSimple "127.0.0.1" (fromIntegral port) False
  client <- assertRight =<< createGrpcClient cfg

  actualResponse <-
    runExceptT (runFederatorClientWith client targetDomain originDomain action)
      `finally` Async.cancel federatorThread
  reqs <- readIORef requestsRef
  pure (actualResponse, reqs)
  where
    targetDomain = Domain "target.example.com"
    originDomain = Domain "origin.example.com"
    isPortOpen :: Int -> IO Bool
    isPortOpen port = do
      let sockAddr = SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1))
      bracket (socket AF_INET Stream 6 {- TCP -}) close' $ \sock -> do
        portRes <- try $ connect sock sockAddr
        case portRes of
          Right () -> return True
          Left e ->
            if (Errno <$> ioe_errno e) == Just eCONNREFUSED
              then return False
              else throwIO e

newtype MockT m a = MockT {unMock :: ReaderT (IORef RecievedRequests) m a}
  deriving newtype (Functor, Applicative, Monad, MonadReader (IORef RecievedRequests), MonadIO)

instance (MonadError ServerError m) => MonadError ServerError (MockT m) where
  throwError err = MockT . lift $ throwError err
  catchError action f = do
    r <- ask
    MockT . lift $ catchError (runMockT r action) (runMockT r . f)

instance MonadIO m => MonadState RecievedRequests (MockT m) where
  get = readIORef =<< ask
  put x = do
    ref <- ask
    writeIORef ref x

runMockT :: IORef RecievedRequests -> MockT m a -> m a
runMockT ref mock = runReaderT (unMock mock) ref

retryWhileN :: forall a m. (MonadIO m) => Int -> (a -> Bool) -> m a -> m a
retryWhileN n f m =
  retrying
    (constantDelay 1000 <> limitRetries n)
    (const (return . f))
    (const m)

assertRight :: Show a => Either a b -> IO b
assertRight = \case
  Left a -> do
    expectationFailure $ "Expected Right, got Left: " <> show a
    error "impossible"
  Right b -> pure b

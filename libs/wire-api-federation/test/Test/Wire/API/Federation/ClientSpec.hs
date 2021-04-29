{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Test.Wire.API.Federation.ClientSpec where

import qualified Control.Concurrent.Async as Async
import Control.Exception (bracket, finally, throwIO, try)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.State (MonadState (..), modify)
import Control.Retry (constantDelay, limitRetries, retrying)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Domain (Domain (Domain))
import Foreign.C (Errno (Errno), eCONNREFUSED)
import GHC.IO.Exception (ioe_errno)
import Imports
import Mu.GRpc.Client.Record (grpcClientConfigSimple)
import Mu.GRpc.Server (msgProtoBuf, runGRpcAppTrans)
import Mu.Server (MonadServer, ServerError, ServerErrorIO, SingleServerT)
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

    -- This behaviour is actually incorrect, the fix in upstream is not merged
    -- yet: https://github.com/haskell-grpc-native/http2-grpc-haskell/pull/48
    actualResponse `shouldBe` Left (FederationClientRPCError "grpc error: not enough bytes")

-- * GRPC Server Mocking Machinery

type RecievedRequests = [FederatedRequest]

outwardService :: IO OutwardResponse -> SingleServerT info Outward (MockT ServerErrorIO) _
outwardService res = Mu.singleService (Mu.method @"call" (callOutward res))

callOutward :: (MonadServer m, MonadState RecievedRequests m) => IO OutwardResponse -> FederatedRequest -> m OutwardResponse
callOutward res req = do
  modify (<> [req])
  liftIO res

mkSuccessResponse :: Aeson.ToJSON a => a -> IO OutwardResponse
mkSuccessResponse = pure . OutwardResponseBody . LBS.toStrict . Aeson.encode

mkErrorResponse :: OutwardError -> IO OutwardResponse
mkErrorResponse = pure . OutwardResponseError

-- This is mostly copy-pasta from brig-integration. Perhaps this should be part
-- of the wire-api-federation libarary?
withMockFederator :: IO OutwardResponse -> FederatorClient 'Brig (ExceptT FederationClientError IO) a -> IO (Either FederationClientError a, RecievedRequests)
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

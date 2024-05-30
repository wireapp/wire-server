module Network.Wai.Utilities.ServerSpec where

import Data.ByteString.Char8 qualified as BC8
import Imports
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Internal
import Network.Wai.Utilities.Server
import System.IO.Temp
import System.Logger qualified as Log
import Test.Hspec

spec :: Spec
spec = do
  describe "requestIdMiddleware" $ do
    it "should add request id header if it is missing in the orig request" $ do
      requestProcessed <- newIORef False
      reqIdRef <- newIORef Nothing
      withSystemTempFile "requestIdMiddlewareTest-" $ \logFile logFileHandle -> do
        hClose logFileHandle
        logger <- Log.new $ Log.setOutput (Log.Path logFile) Log.defSettings
        let headerName = "Request-ID-Test"
            app req responder = do
              writeIORef requestProcessed True
              case find (\(n, _) -> n == headerName) (requestHeaders req) of
                Nothing -> expectationFailure "The request has no header with a request ID"
                Just (_, reqId) -> writeIORef reqIdRef (Just reqId)
              responder $ responseLBS status200 [] ""
            req0 = defaultRequest {requestMethod = "POST", rawPathInfo = "/req-id-test"}
            responder0 _resp = pure ResponseReceived
        void $ requestIdMiddleware logger headerName app req0 responder0

        Log.close logger
        logEntries <- readFile logFile

        Just reqId <- readIORef reqIdRef
        length (lines logEntries) `shouldBe` 1
        logEntries `shouldContain` "generated a new request id for local request"
        logEntries `shouldContain` ("request=" <> BC8.unpack reqId)
        logEntries `shouldContain` "method=POST"
        logEntries `shouldContain` "path=/req-id-test"

        readIORef requestProcessed `shouldReturn` True

    it "should not add request id header if is present in the orig request" $ do
      requestProcessed <- newIORef False
      withSystemTempFile "requestIdMiddlewareTest-" $ \logFile logFileHandle -> do
        hClose logFileHandle
        logger <- Log.new $ Log.setOutput (Log.Path logFile) Log.defSettings
        let origRequestId = "test-req-id"
            headerName = "Request-ID-Test"
            app req responder = do
              writeIORef requestProcessed True
              case find (\(n, _) -> n == headerName) (requestHeaders req) of
                Nothing -> expectationFailure "The request has no header with a request ID"
                Just (_, foundReqId) -> foundReqId `shouldBe` origRequestId
              responder $ responseLBS status200 [] ""
            req0 = defaultRequest {requestHeaders = [(headerName, origRequestId)]}
            responder0 _resp = pure ResponseReceived
        void $ requestIdMiddleware logger headerName app req0 responder0
        Log.close logger

        -- Nothing should be logged
        logEntries <- readFile logFile
        length logEntries `shouldBe` 0

        readIORef requestProcessed `shouldReturn` True

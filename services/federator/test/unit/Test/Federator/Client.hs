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

module Test.Federator.Client (tests) where

import Control.Exception hiding (handle)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (byteString)
import Data.Domain
import Federator.MockServer
import Imports
import Network.HTTP.Types as HTTP
import qualified Network.HTTP2.Client as HTTP2
import qualified Network.Wai as Wai
import qualified Network.Wai.Utilities.Error as Wai
import Servant.Client.Core
import Servant.Types.SourceT
import Test.QuickCheck (arbitrary, generate)
import Test.Tasty
import Test.Tasty.HUnit
import Util.Options
import Wire.API.Federation.API
import Wire.API.Federation.API.Brig
import Wire.API.Federation.Client
import Wire.API.Federation.Component
import Wire.API.Federation.Error
import Wire.API.User (UserProfile)

targetDomain :: Domain
targetDomain = Domain "target.example.com"

originDomain :: Domain
originDomain = Domain "origin.example.com"

defaultHeaders :: [HTTP.Header]
defaultHeaders = [("Content-Type", "application/json")]

tests :: TestTree
tests =
  testGroup
    "Federator.Client"
    [ testGroup
        "Servant"
        [ testCase "testClientSuccess" testClientSuccess,
          testCase "testClientFailure" testClientFailure,
          testCase "testFederatorFailure" testFederatorFailure,
          testCase "testClientException" testClientExceptions,
          testCase "testClientConnectionError" testClientConnectionError
        ],
      testGroup
        "HTTP2 client"
        [ testCase "testResponseHeaders" testResponseHeaders,
          testCase "testStreaming" testStreaming
        ]
    ]

newtype ResponseFailure = ResponseFailure Wai.Error
  deriving (Show)

withMockFederatorClient ::
  KnownComponent c =>
  [HTTP.Header] ->
  (FederatedRequest -> IO LByteString) ->
  FederatorClient c a ->
  IO (Either ResponseFailure a, [FederatedRequest])
withMockFederatorClient headers resp action = withTempMockFederator headers resp $ \port -> do
  let env =
        FederatorClientEnv
          { ceOriginDomain = originDomain,
            ceTargetDomain = targetDomain,
            ceFederator = Endpoint "127.0.0.1" (fromIntegral port)
          }
  a <- runFederatorClient env action
  case a of
    Left (FederatorClientError r) -> pure (Left (ResponseFailure r))
    Left err -> assertFailure $ "Unexpected client error: " <> displayException err
    Right x -> pure (Right x)

testClientSuccess :: IO ()
testClientSuccess = do
  handle <- generate arbitrary
  expectedResponse :: UserProfile <- generate arbitrary

  (actualResponse, sentRequests) <-
    withMockFederatorClient defaultHeaders (const (pure (Aeson.encode (Just expectedResponse)))) $
      getUserByHandle clientRoutes handle

  sentRequests
    @?= [ FederatedRequest
            { frTargetDomain = targetDomain,
              frBody = Aeson.encode handle,
              frOriginDomain = originDomain,
              frRPC = "get-user-by-handle",
              frComponent = Brig
            }
        ]
  first (const ()) actualResponse @?= Right (Just expectedResponse)

testClientFailure :: IO ()
testClientFailure = do
  handle <- generate arbitrary

  (actualResponse, _) <-
    withMockFederatorClient
      defaultHeaders
      (const (throw (MockErrorResponse HTTP.status422 "wrong domain")))
      $ do
        getUserByHandle clientRoutes handle

  case actualResponse of
    Right _ -> assertFailure "unexpected success"
    Left (ResponseFailure werr) -> do
      Wai.code werr @?= HTTP.status422
      Wai.message werr @?= "wrong domain"

testFederatorFailure :: IO ()
testFederatorFailure = do
  handle <- generate arbitrary

  (actualResponse, _) <-
    withMockFederatorClient
      defaultHeaders
      (const (throw (MockErrorResponse HTTP.status403 "invalid path")))
      $ do
        getUserByHandle clientRoutes handle

  case actualResponse of
    Right _ -> assertFailure "unexpected success"
    Left (ResponseFailure werr) -> do
      Wai.code werr @?= HTTP.status500
      Wai.label werr @?= "federation-local-error"

testClientExceptions :: IO ()
testClientExceptions = do
  handle <- generate arbitrary

  (response, _) <-
    withMockFederatorClient defaultHeaders (const (evaluate (error "unhandled exception"))) $
      getUserByHandle clientRoutes handle

  case response of
    Right _ -> assertFailure "unexpected success"
    Left (ResponseFailure werr) -> Wai.code werr @?= HTTP.status500

testClientConnectionError :: IO ()
testClientConnectionError = do
  handle <- generate arbitrary
  let env =
        FederatorClientEnv
          { ceOriginDomain = originDomain,
            ceTargetDomain = targetDomain,
            ceFederator = Endpoint "127.0.0.1" 1
          }
  result <- runFederatorClient env (getUserByHandle clientRoutes handle)
  case result of
    Left (FederatorClientHTTP2Error (FederatorClientConnectionError _)) -> pure ()
    Left x -> assertFailure $ "Expected connection error, got: " <> show x
    Right _ -> assertFailure "Expected connection with the server to fail"

testResponseHeaders :: IO ()
testResponseHeaders = do
  (r, _) <- withTempMockFederator [("X-Foo", "bar")] (const mempty) $ \port -> do
    let req =
          HTTP2.requestBuilder
            HTTP.methodPost
            "/rpc/target.example.com/brig/test"
            [("Wire-Origin-Domain", "origin.example.com")]
            "body"
    performHTTP2Request Nothing req "127.0.0.1" port
  case r of
    Left err ->
      assertFailure $
        "Unexpected error while connecting to mock federator: " <> show err
    Right resp -> do
      responseStatusCode resp @?= HTTP.status200
      lookup "X-Foo" (toList (responseHeaders resp)) @?= Just "bar"

testStreaming :: IO ()
testStreaming = bracket (startMockServer Nothing app) fst $ \(_, port) -> do
  let req = HTTP2.requestBuilder HTTP.methodPost "test" [] mempty
  withHTTP2Request Nothing req "127.0.0.1" port $ \resp -> do
    let expected = "Hello\nHello\nHello\n"
    actual <- takeSourceT (BS.length expected) (responseBody resp)
    actual @?= expected
  where
    takeStepT :: Monad m => Int -> StepT m ByteString -> m ByteString
    takeStepT _ Stop = pure mempty
    takeStepT _ (Error _) = pure mempty
    takeStepT s (Skip next) = takeStepT s next
    takeStepT s (Yield chunk next)
      | BS.length chunk >= s =
        pure (BS.take s chunk)
      | otherwise =
        fmap
          (chunk <>)
          (takeStepT (s - BS.length chunk) next)
    takeStepT s (Effect m) = m >>= takeStepT s

    takeSourceT :: Monad m => Int -> SourceT m ByteString -> m ByteString
    takeSourceT s m = unSourceT m (takeStepT s)

    app _ k = k $
      Wai.responseStream HTTP.ok200 mempty $ \write flush ->
        let go = write (byteString "Hello\n") *> flush *> go in go

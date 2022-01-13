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
import Control.Monad.Codensity
import Control.Monad.Except
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder, byteString, toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Domain
import Data.Proxy
import qualified Data.Text.Encoding as Text
import Federator.MockServer
import Imports
import Network.HTTP.Media
import Network.HTTP.Types as HTTP
import qualified Network.HTTP2.Client as HTTP2
import qualified Network.Wai as Wai
import qualified Network.Wai.Utilities.Error as Wai
import Servant.API
import Servant.Client
import Servant.Client.Core
import Servant.Types.SourceT
import Test.QuickCheck (arbitrary, generate)
import Test.Tasty
import Test.Tasty.HUnit
import Util.Options
import Wire.API.Federation.API
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
          testCase "testClientStreaming" testClientStreaming,
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
  (FederatedRequest -> IO (MediaType, LByteString)) ->
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
    withMockFederatorClient
      defaultHeaders
      (const (pure ("application/json", Aeson.encode (Just expectedResponse))))
      $ fedClient @'Brig @VL @"get-user-by-handle" handle

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

type StreamingAPI = StreamGet NewlineFraming PlainText (SourceIO Text)

testClientStreaming :: IO ()
testClientStreaming = withInfiniteMockServer $ \port -> do
  let env =
        FederatorClientEnv
          { ceOriginDomain = originDomain,
            ceTargetDomain = targetDomain,
            ceFederator = Endpoint "127.0.0.1" (fromIntegral port)
          }
  let c = clientIn (Proxy @StreamingAPI) (Proxy @(FederatorClient 'Brig))
  runCodensity (runFederatorClientToCodensity env c) $ \case
    Left err -> assertFailure $ "Unexpected error: " <> displayException err
    Right out -> do
      let expected = mconcat (replicate 500 "Hello")
      actual <- takeSourceT (fromIntegral (LBS.length expected)) (fmap Text.encodeUtf8 out)
      actual @?= expected

testClientFailure :: IO ()
testClientFailure = do
  handle <- generate arbitrary

  (actualResponse, _) <-
    withMockFederatorClient
      defaultHeaders
      (const (throw (MockErrorResponse HTTP.status422 "wrong domain")))
      $ do
        fedClient @'Brig @VL @"get-user-by-handle" handle

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
        fedClient @'Brig @VL @"get-user-by-handle" handle

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
      fedClient @'Brig @VL @"get-user-by-handle" handle

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
  result <- runFederatorClient env (fedClient @'Brig @VL @"get-user-by-handle" handle)
  case result of
    Left (FederatorClientHTTP2Error (FederatorClientConnectionError _)) -> pure ()
    Left x -> assertFailure $ "Expected connection error, got: " <> show x
    Right _ -> assertFailure "Expected connection with the server to fail"

testResponseHeaders :: IO ()
testResponseHeaders = do
  (r, _) <- withTempMockFederator
    [("X-Foo", "bar")]
    (const $ pure ("application" // "json", mempty))
    $ \port -> do
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
testStreaming = withInfiniteMockServer $ \port -> do
  let req = HTTP2.requestBuilder HTTP.methodPost "test" [] mempty
  withHTTP2Request Nothing req "127.0.0.1" port $ \resp -> do
    let expected = mconcat (replicate 512 "Hello\n")
    actual <- takeSourceT (fromIntegral (LBS.length expected)) (responseBody resp)
    actual @?= expected

withInfiniteMockServer :: (Int -> IO a) -> IO a
withInfiniteMockServer k = bracket (startMockServer Nothing app) fst (k . snd)
  where
    app _ respond = respond $
      Wai.responseStream HTTP.ok200 mempty $ \write flush ->
        let go n = do
              when (n == 0) flush
              write (byteString "Hello\n") *> go (if n == 0 then 100 else n - 1)
         in go (1000 :: Int)

-- SourceT utilities

takeStepT :: Builder -> Int -> StepT IO ByteString -> IO LByteString
takeStepT acc _ Stop = pure (toLazyByteString acc)
takeStepT acc _ (Error _) = pure (toLazyByteString acc)
takeStepT acc s (Skip next) = takeStepT acc s next
takeStepT acc s (Yield chunk next)
  | BS.length chunk >= s =
    pure $ toLazyByteString (acc <> byteString (BS.take s chunk))
  | otherwise = do
    takeStepT (acc <> byteString chunk) (s - BS.length chunk) next
takeStepT acc s (Effect m) = m >>= takeStepT acc s

takeSourceT :: Int -> SourceT IO ByteString -> IO LByteString
takeSourceT s m = unSourceT m (takeStepT mempty s)

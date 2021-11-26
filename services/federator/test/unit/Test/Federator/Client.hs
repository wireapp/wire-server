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
import Data.Domain
import Federator.MockServer
import Imports
import Network.HTTP.Types as HTTP
import Network.Wai.Utilities.Error as Wai
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

tests :: TestTree
tests =
  testGroup
    "Federator.Client"
    [ testCase "testClientSuccess" testClientSuccess,
      testCase "testClientFailure" testClientFailure,
      testCase "testClientException" testClientExceptions,
      testCase "testClientConnectionError" testClientConnectionError
    ]

newtype ResponseFailure = ResponseFailure Wai.Error
  deriving (Show)

withMockFederatorClient ::
  KnownComponent c =>
  (FederatedRequest -> IO LByteString) ->
  FederatorClient c a ->
  IO (Either ResponseFailure a, [FederatedRequest])
withMockFederatorClient resp action = withTempMockFederator resp $ \port -> do
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
    withMockFederatorClient (const (pure (Aeson.encode (Just expectedResponse)))) $
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
    withMockFederatorClient (const (throw (MockErrorResponse HTTP.status403 "mock error"))) $ do
      getUserByHandle clientRoutes handle

  case actualResponse of
    Right _ -> assertFailure "unexpected success"
    Left (ResponseFailure werr) -> do
      Wai.code werr @?= HTTP.status403
      Wai.message werr @?= "mock error"

testClientExceptions :: IO ()
testClientExceptions = do
  handle <- generate arbitrary

  (response, _) <-
    withMockFederatorClient (const (evaluate (error "unhandled exception"))) $
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

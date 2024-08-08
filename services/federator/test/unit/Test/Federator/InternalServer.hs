{-# OPTIONS_GHC -Wno-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Federator.InternalServer (tests) where

import Data.ByteString.Builder
import Data.ByteString.Conversion
import Data.Default
import Data.Domain
import Federator.Error.ServerError
import Federator.InternalServer (callOutward)
import Federator.Metrics
import Federator.RPC
import Federator.Remote
import Federator.Validation
import Imports
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Internal qualified as Wai
import Network.Wai.Utilities.Server (federationRequestIdHeaderName)
import Network.Wai.Utilities.Server qualified as Wai
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Servant.Client.Core
import Servant.Types.SourceT
import Test.Federator.Options (noClientCertSettings)
import Test.Federator.Util
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Federation.Component
import Wire.API.Federation.Domain
import Wire.API.Routes.FederationDomainConfig
import Wire.API.User.Search
import Wire.Sem.Logger.TinyLog

tests :: TestTree
tests =
  testGroup
    "Federate"
    [ testGroup
        "with remote"
        [ federatedRequestSuccess,
          federatedRequestFailureAllowList
        ]
    ]

federatedRequestSuccess :: TestTree
federatedRequestSuccess =
  testCase "should successfully return success response" $ do
    let settings = noClientCertSettings
    let targetDomain = Domain "target.example.com"
        requestHeaders = [(originDomainHeaderName, "origin.example.com")]
    request <-
      testRequest
        def
          { trPath = "/rpc/" <> toByteString' targetDomain <> "/brig/get-user-by-handle",
            trBody = "\"foo\"",
            trExtraHeaders = requestHeaders
          }
    let verifyCallAndRespond :: (Member (Embed IO) r) => Sem (Remote ': r) a -> Sem r a
        verifyCallAndRespond = interpret $ \case
          DiscoverAndCall domain component rpc headers body -> embed @IO $ do
            domain @?= targetDomain
            component @?= Brig
            rpc @?= "get-user-by-handle"
            sort headers @?= sort (requestHeaders <> [(federationRequestIdHeaderName, "test")])
            toLazyByteString body @?= "\"foo\""
            pure
              Response
                { responseStatusCode = HTTP.ok200,
                  responseHeaders = mempty,
                  responseHttpVersion = HTTP.http20,
                  responseBody = source ["\"bar\""]
                }

    let assertMetrics :: (Member (Embed IO) r) => Sem (Metrics ': r) a -> Sem r a
        assertMetrics = interpret $ \case
          OutgoingCounterIncr td -> embed @IO $ td @?= targetDomain
          IncomingCounterIncr _ -> embed @IO $ assertFailure "Should not increment incoming counter"

    resRef <- newIORef Nothing
    let saveResponse res = writeIORef resRef (Just res) $> Wai.ResponseReceived
    _ <-
      runM
        . verifyCallAndRespond
        . assertNoError @ValidationError
        . assertNoError @ServerError
        . discardTinyLogs
        . runInputConst settings
        . runInputConst (FederationDomainConfigs AllowDynamic [FederationDomainConfig (Domain "target.example.com") FullSearch FederationRestrictionAllowAll] 10)
        . assertMetrics
        $ callOutward targetDomain Brig (RPC "get-user-by-handle") request saveResponse
    Just res <- readIORef resRef
    Wai.responseStatus res @?= HTTP.status200
    body <- Wai.lazyResponseBody res
    body @?= "\"bar\""

-- @SF.Federation @TSFI.Federate @TSFI.DNS @S2 @S3 @S7
--
-- Refuse to send outgoing request to non-included domain when AllowDynamic is configured.
federatedRequestFailureAllowList :: TestTree
federatedRequestFailureAllowList =
  testCase "federatedRequestFailureAllowList - should not make a call when target domain not in the allow list" $ do
    let settings = noClientCertSettings
    let targetDomain = Domain "target.example.com"
        headers = [(originDomainHeaderName, "origin.example.com")]
    request <-
      testRequest
        def
          { trPath = "/rpc/" <> toByteString' targetDomain <> "/brig/get-user-by-handle",
            trBody = "\"foo\"",
            trExtraHeaders = headers
          }

    let checkRequest :: Sem (Remote ': r) a -> Sem r a
        checkRequest = interpret $ \case
          DiscoverAndCall {} ->
            pure
              Response
                { responseStatusCode = HTTP.ok200,
                  responseHeaders = mempty,
                  responseHttpVersion = HTTP.http20,
                  responseBody = source ["\"bar\""]
                }
    let interpretMetricsEmpty = interpret $ \case
          OutgoingCounterIncr _ -> pure ()
          IncomingCounterIncr _ -> pure ()

    eith <-
      runM
        . runError @ValidationError
        . void
        . checkRequest
        . assertNoError @ServerError
        . discardTinyLogs
        . runInputConst settings
        . runInputConst (FederationDomainConfigs AllowDynamic [FederationDomainConfig (Domain "hello.world") FullSearch FederationRestrictionAllowAll] 10)
        . interpretMetricsEmpty
        $ callOutward targetDomain Brig (RPC "get-user-by-handle") request undefined
    eith @?= Left (FederationDenied targetDomain)

-- @END

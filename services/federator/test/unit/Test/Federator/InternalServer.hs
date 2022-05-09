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

import qualified Data.Aeson as Aeson
import Data.ByteString.Builder
import Data.ByteString.Conversion
import Data.Default
import Data.Domain
import Federator.Error.ServerError
import Federator.InternalServer (callOutward)
import Federator.Options (AllowedDomains (..), FederationStrategy (..), RunSettings (..))
import Federator.Remote
import Federator.Validation
import Imports
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Utilities.Server as Wai
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
import Wire.API.Federation.Version
import Wire.Sem.Logger.TinyLog

tests :: TestTree
tests =
  testGroup
    "Federate with remote"
    [ federatedRequestSuccess,
      federatedRequestFailureAllowList,
      federatedAPIVersion
    ]

settingsWithAllowList :: [Domain] -> RunSettings
settingsWithAllowList domains =
  noClientCertSettings {federationStrategy = AllowList (AllowedDomains domains)}

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
    let interpretCall :: Member (Embed IO) r => Sem (Remote ': r) a -> Sem r a
        interpretCall = interpret $ \case
          DiscoverAndCall domain component rpc headers body -> embed @IO $ do
            domain @?= targetDomain
            component @?= Brig
            rpc @?= "get-user-by-handle"
            headers @?= requestHeaders
            toLazyByteString body @?= "\"foo\""
            pure
              Response
                { responseStatusCode = HTTP.ok200,
                  responseHeaders = mempty,
                  responseHttpVersion = HTTP.http20,
                  responseBody = source ["\"bar\""]
                }
          GetAPIVersions _ -> embed @IO $ assertFailure "Unexpected use of getAPIVersion"
    res <-
      runM
        . interpretCall
        . assertNoError @ValidationError
        . assertNoError @ServerError
        . discardTinyLogs
        . runInputConst settings
        $ callOutward request
    Wai.responseStatus res @?= HTTP.status200
    body <- Wai.lazyResponseBody res
    body @?= "\"bar\""

-- @SF.Federation @TSFI.Federate @TSFI.DNS @S2 @S3 @S7
--
-- Refuse to send outgoing request to non-included domain when allowlist is configured.
federatedRequestFailureAllowList :: TestTree
federatedRequestFailureAllowList =
  testCase "should not make a call when target domain not in the allowList" $ do
    let settings = settingsWithAllowList [Domain "hello.world"]
    let targetDomain = Domain "target.example.com"
        headers = [(originDomainHeaderName, "origin.example.com")]
    request <-
      testRequest
        def
          { trPath = "/rpc/" <> toByteString' targetDomain <> "/brig/get-user-by-handle",
            trBody = "\"foo\"",
            trExtraHeaders = headers
          }

    let checkRequest :: Member (Embed IO) r => Sem (Remote ': r) a -> Sem r a
        checkRequest = interpret $ \case
          DiscoverAndCall {} ->
            pure
              Response
                { responseStatusCode = HTTP.ok200,
                  responseHeaders = mempty,
                  responseHttpVersion = HTTP.http20,
                  responseBody = source ["\"bar\""]
                }
          GetAPIVersions _ -> embed @IO $ assertFailure "Unexpected use of getAPIVersion"

    eith <-
      runM
        . runError @ValidationError
        . void
        . checkRequest
        . assertNoError @ServerError
        . discardTinyLogs
        . runInputConst settings
        $ callOutward request
    eith @?= Left (FederationDenied targetDomain)

-- @END

federatedAPIVersion :: TestTree
federatedAPIVersion = testCase "should make an API version query" $ do
  let settings = noClientCertSettings
      targetDomain = Domain "target.example.com"
      headers = [(originDomainHeaderName, "origin.example.com")]
      vinfo = Aeson.encode (VersionInfo (toList supportedVersions))
  request <-
    testRequest
      def
        { trPath = "/api-version/" <> toByteString' targetDomain,
          trMethod = HTTP.methodGet,
          trExtraHeaders = headers
        }

  let interpretCall :: Member (Embed IO) r => Sem (Remote ': r) a -> Sem r a
      interpretCall = interpret $ \case
        DiscoverAndCall {} -> embed @IO $ assertFailure "Unexpected RPC call"
        GetAPIVersions domain -> do
          embed @IO $ domain @?= targetDomain
          pure vinfo

  res <-
    runM
      . interpretCall
      . assertNoError @ValidationError
      . assertNoError @ServerError
      . discardTinyLogs
      . runInputConst settings
      $ callOutward request
  Wai.responseStatus res @?= HTTP.status200
  body <- Wai.lazyResponseBody res
  body @?= vinfo

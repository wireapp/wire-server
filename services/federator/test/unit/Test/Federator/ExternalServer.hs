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
{-# OPTIONS_GHC -Wno-orphans -fprint-potential-instances #-}

module Test.Federator.ExternalServer where

import qualified Data.ByteString as BS
import Data.Default
import Data.Domain
import Data.String.Conversions (cs)
import qualified Data.Text.Encoding as Text
import Federator.Discovery
import Federator.Error.ServerError (ServerError (..))
import Federator.ExternalServer
import Federator.Service (Service (..), ServiceStreaming)
import Federator.Validation
import Imports
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Utilities.Server as Wai
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Output
import Polysemy.TinyLog
import qualified Servant.Client.Core as Servant
import Servant.Types.SourceT
import Test.Federator.Options (noClientCertSettings)
import Test.Federator.Util
import Test.Federator.Validation (mockDiscoveryTrivial)
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Federation.Component

tests :: TestTree
tests =
  testGroup
    "ExternalServer"
    [ requestBrigSuccess,
      requestBrigFailure,
      requestGalleySuccess,
      requestNoCertificate,
      requestNoDomain,
      testInvalidPaths,
      testInvalidComponent,
      testMethod
    ]

exampleRequest :: FilePath -> ByteString -> IO Wai.Request
exampleRequest certFile path = do
  cert <- BS.readFile certFile
  testRequest
    def
      { trPath = path,
        trDomainHeader = Just (Text.encodeUtf8 exampleDomain),
        trCertificateHeader = Just cert,
        trBody = "\"foo\""
      }

data Call = Call
  { cComponent :: Component,
    cPath :: ByteString,
    cBody :: LByteString,
    cDomain :: Domain
  }
  deriving (Eq, Show)

mockService ::
  Members [Output Call, Embed IO] r =>
  HTTP.Status ->
  Sem (ServiceStreaming ': r) a ->
  Sem r a
mockService status = interpret $ \case
  ServiceCall comp path body domain -> do
    output (Call comp path body domain)
    pure
      Servant.Response
        { Servant.responseStatusCode = status,
          Servant.responseHeaders = mempty,
          Servant.responseHttpVersion = HTTP.http11,
          Servant.responseBody = source ["\"bar\""]
        }

requestBrigSuccess :: TestTree
requestBrigSuccess =
  testCase "should forward response from brig when status is 200" $ do
    request <-
      exampleRequest
        "test/resources/unit/localhost.example.com.pem"
        "/federation/brig/get-user-by-handle"
    (actualCalls, res) <-
      runM
        . runOutputList
        . mockService HTTP.ok200
        . assertNoError @ValidationError
        . assertNoError @DiscoveryFailure
        . assertNoError @ServerError
        . discardLogs
        . mockDiscoveryTrivial
        . runInputConst noClientCertSettings
        $ callInward request
    let expectedCall = Call Brig "/federation/get-user-by-handle" "\"foo\"" aValidDomain
    assertEqual "one call to brig should be made" [expectedCall] actualCalls
    Wai.responseStatus res @?= HTTP.status200
    body <- Wai.lazyResponseBody res
    body @?= "\"bar\""

requestBrigFailure :: TestTree
requestBrigFailure =
  testCase "should preserve the status code returned by the service" $ do
    request <-
      exampleRequest
        "test/resources/unit/localhost.example.com.pem"
        "/federation/brig/get-user-by-handle"

    (actualCalls, res) <-
      runM
        . runOutputList
        . mockService HTTP.notFound404
        . assertNoError @ValidationError
        . assertNoError @DiscoveryFailure
        . assertNoError @ServerError
        . discardLogs
        . mockDiscoveryTrivial
        . runInputConst noClientCertSettings
        $ callInward request

    let expectedCall = Call Brig "/federation/get-user-by-handle" "\"foo\"" aValidDomain
    assertEqual "one call to brig should be made" [expectedCall] actualCalls
    Wai.responseStatus res @?= HTTP.notFound404
    body <- Wai.lazyResponseBody res
    body @?= "\"bar\""

requestGalleySuccess :: TestTree
requestGalleySuccess =
  testCase "should forward response from galley when response has status 200" $ do
    request <-
      exampleRequest
        "test/resources/unit/localhost.example.com.pem"
        "/federation/galley/get-conversations"

    runM $ do
      (actualCalls, res) <-
        runOutputList
          . mockService HTTP.ok200
          . assertNoError @ValidationError
          . assertNoError @DiscoveryFailure
          . assertNoError @ServerError
          . discardLogs
          . mockDiscoveryTrivial
          . runInputConst noClientCertSettings
          $ callInward request
      let expectedCall = Call Galley "/federation/get-conversations" "\"foo\"" aValidDomain
      embed $ assertEqual "one call to galley should be made" [expectedCall] actualCalls
      embed $ Wai.responseStatus res @?= HTTP.status200
      body <- embed $ Wai.lazyResponseBody res
      embed $ body @?= "\"bar\""

requestNoDomain :: TestTree
requestNoDomain =
  testCase "should fail with a ServerError when no origin domain header is given" $ do
    cert <- BS.readFile "test/resources/unit/localhost.example.com.pem"
    request <-
      testRequest
        def
          { trCertificateHeader = Just cert,
            trPath = "/federation/brig/get-users"
          }

    runM $ do
      (actualCalls, res) <-
        runOutputList @Call
          . mockService HTTP.ok200
          . runError
          . assertNoError @ValidationError
          . assertNoError @DiscoveryFailure
          . discardLogs
          . mockDiscoveryTrivial
          . runInputConst noClientCertSettings
          $ callInward request

      embed $ assertEqual "no calls to services should be made" [] actualCalls
      embed $ void res @?= Left NoOriginDomain

requestNoCertificate :: TestTree
requestNoCertificate =
  testCase "should fail with a ValidationError when no certificate is given" $ do
    request <-
      testRequest
        def
          { trDomainHeader = Just (Text.encodeUtf8 exampleDomain),
            trPath = "/federation/brig/get-users"
          }

    (actualCalls, res) <-
      runM
        . runOutputList @Call
        . mockService HTTP.ok200
        . runError
        . assertNoError @ServerError
        . assertNoError @DiscoveryFailure
        . discardLogs
        . mockDiscoveryTrivial
        . runInputConst noClientCertSettings
        $ callInward request

    assertEqual "no calls to services should be made" [] actualCalls
    void res @?= Left NoClientCertificate

testInvalidPaths :: TestTree
testInvalidPaths = do
  testCase "should not forward requests with invalid paths to services" $ do
    let invalidPaths =
          [ "",
            "/",
            "///",
            -- disallowed paths
            "federation",
            "/federation",
            "/federation/",
            "/federation/brig",
            "/federation/brig/", -- empty component
            "i/users",
            "/i/users",
            "/federation/brig/too/many/components",
            -- syntax we don't wish to support
            "http://federation.wire.link/federation/galley", -- contains scheme and domain
            "http://federation/stuff", -- contains scheme
            "federation.wire.link/federation/brig/stuff", -- contains domain
            "/federation/brig/rpc?bar[]=baz", -- queries not allowed
            "/federation/brig/stuff?key=value", -- queries not allowed
            -- rpc names that don't match [0-9a-zA-Z-_]+
            "/federation/brig/%2e%2e/i/users", -- percent-encoded '../'
            "/federation/brig/%2E%2E/i/users",
            "/federation/brig/..%2Fi%2Fusers", -- percent-encoded ../i/users
            "/federation/brig/%252e%252e/i/users", -- double percent-encoded '../'
            "/federation/brig/%c0%ae%c0%ae/i/users" -- weird-encoded '../'
          ]

    for_ invalidPaths $ \invalidPath -> do
      request <-
        exampleRequest
          "test/resources/unit/localhost.example.com.pem"
          invalidPath

      (actualCalls, res) <-
        runM
          . runOutputList @Call
          . mockService HTTP.ok200
          . runError @ServerError
          . assertNoError @ValidationError
          . assertNoError @DiscoveryFailure
          . discardLogs
          . mockDiscoveryTrivial
          . runInputConst noClientCertSettings
          $ callInward request

      assertEqual ("Expected request with path \"" <> cs invalidPath <> "\" to fail") (Left InvalidRoute) (void res)
      assertEqual "no calls to any service should be made" [] actualCalls

testInvalidComponent :: TestTree
testInvalidComponent =
  testCase "a path with an invalid component should result in an error" $ do
    request <-
      exampleRequest
        "test/resources/unit/localhost.example.com.pem"
        "/federation/mast/get-users"

    (actualCalls, res) <-
      runM
        . runOutputList @Call
        . mockService HTTP.ok200
        . runError @ServerError
        . assertNoError @ValidationError
        . assertNoError @DiscoveryFailure
        . discardLogs
        . mockDiscoveryTrivial
        . runInputConst noClientCertSettings
        $ callInward request

    void res @?= Left (UnknownComponent "mast")
    assertEqual "no calls to any service should be made" [] actualCalls

testMethod :: TestTree
testMethod =
  testCase "only POST should be supported" $ do
    cert <- BS.readFile "test/resources/unit/localhost.example.com.pem"
    let tr =
          def
            { trPath = "/federation/galley/send-message",
              trDomainHeader = Just (Text.encodeUtf8 exampleDomain),
              trCertificateHeader = Just cert,
              trBody = "\"hello\""
            }

    for_ [HTTP.methodGet, HTTP.methodDelete, HTTP.methodPut, HTTP.methodPatch] $ \method -> do
      request <- testRequest tr {trMethod = method}
      res <-
        runM
          . runError @ServerError
          . interpret @ServiceStreaming (\_ -> embed $ assertFailure "unexpected call to service")
          . assertNoError @ValidationError
          . assertNoError @DiscoveryFailure
          . discardLogs
          . mockDiscoveryTrivial
          . runInputConst noClientCertSettings
          $ callInward request
      void res @?= Left InvalidRoute

exampleDomain :: Text
exampleDomain = "localhost.example.com"

aValidDomain :: Domain
aValidDomain = Domain exampleDomain

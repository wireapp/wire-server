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
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Federator.ExternalServer where

import qualified Data.ByteString as BS
import Data.Default
import Data.Domain
import Data.String.Conversions (cs)
import qualified Data.Text.Encoding as Text
import Federator.Discovery
import Federator.Error.ServerError (ServerError (..))
import Federator.ExternalServer
import Federator.Service (Service)
import Federator.Validation
import Imports
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Utilities.Server as Wai
import Polysemy
import Polysemy.Error
import Polysemy.Input
import qualified Polysemy.TinyLog as TinyLog
import Test.Federator.Options (noClientCertSettings)
import Test.Federator.Util
import Test.Federator.Validation (mockDiscoveryTrivial)
import Test.Polysemy.Mock (Mock (mock), evalMock)
import Test.Polysemy.Mock.TH (genMock)
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Federation.Component

genMock ''Service

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
      testPaths,
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

requestBrigSuccess :: TestTree
requestBrigSuccess =
  testCase "should translate response from brig to 'InwardResponseBody' when response has status 200" $ do
    request <-
      exampleRequest
        "test/resources/unit/localhost.example.com.pem"
        "/federation/brig/get-user-by-handle"
    runM . evalMock @Service @IO $ do
      mockServiceCallReturns @IO (\_ _ _ _ -> pure (HTTP.ok200, Just "\"bar\""))

      res <-
        mock @Service @IO
          . assertNoError @ValidationError
          . assertNoError @DiscoveryFailure
          . assertNoError @ServerError
          . TinyLog.discardLogs
          . mockDiscoveryTrivial
          . runInputConst noClientCertSettings
          $ callInward request
      actualCalls <- mockServiceCallCalls @IO
      let expectedCall = (Brig, "/federation/get-user-by-handle", "\"foo\"", aValidDomain)
      embed $ assertEqual "one call to brig should be made" [expectedCall] actualCalls
      embed $ Wai.responseStatus res @?= HTTP.status200
      body <- embed $ Wai.lazyResponseBody res
      embed $ body @?= "\"bar\""

requestBrigFailure :: TestTree
requestBrigFailure =
  testCase "should translate response from brig to 'InwardResponseError' when response has status 404" $ do
    request <-
      exampleRequest
        "test/resources/unit/localhost.example.com.pem"
        "/federation/brig/get-user-by-handle"

    runM . evalMock @Service @IO $ do
      let brigResponseBody = "response body"
      mockServiceCallReturns @IO (\_ _ _ _ -> pure (HTTP.notFound404, Just brigResponseBody))
      res <-
        mock @Service @IO
          . assertNoError @ValidationError
          . assertNoError @DiscoveryFailure
          . assertNoError @ServerError
          . TinyLog.discardLogs
          . mockDiscoveryTrivial
          . runInputConst noClientCertSettings
          $ callInward request

      actualCalls <- mockServiceCallCalls @IO
      let expectedCall = (Brig, "/federation/get-user-by-handle", "\"foo\"", aValidDomain)
      embed $ assertEqual "one call to brig should be made" [expectedCall] actualCalls
      embed $ Wai.responseStatus res @?= HTTP.notFound404
      body <- embed $ Wai.lazyResponseBody res
      embed $ body @?= brigResponseBody

requestGalleySuccess :: TestTree
requestGalleySuccess =
  testCase "should forward response from galley when response has status 200" $ do
    request <-
      exampleRequest
        "test/resources/unit/localhost.example.com.pem"
        "/federation/galley/get-conversations"

    runM . evalMock @Service @IO $ do
      mockServiceCallReturns @IO (\_ _ _ _ -> pure (HTTP.ok200, Just "\"bar\""))

      res <-
        mock @Service @IO
          . assertNoError @ValidationError
          . assertNoError @DiscoveryFailure
          . assertNoError @ServerError
          . TinyLog.discardLogs
          . mockDiscoveryTrivial
          . runInputConst noClientCertSettings
          $ callInward request
      actualCalls <- mockServiceCallCalls @IO
      let expectedCall = (Galley, "/federation/get-conversations", "\"foo\"", aValidDomain)
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

    runM . evalMock @Service @IO $ do
      mockServiceCallReturns @IO (\_ _ _ _ -> pure (HTTP.ok200, Just "\"bar\""))

      res <-
        runError
          . mock @Service @IO
          . assertNoError @ValidationError
          . assertNoError @DiscoveryFailure
          . TinyLog.discardLogs
          . mockDiscoveryTrivial
          . runInputConst noClientCertSettings
          $ callInward request

      actualCalls <- mockServiceCallCalls @IO
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

    runM . evalMock @Service @IO $ do
      mockServiceCallReturns @IO (\_ _ _ _ -> pure (HTTP.ok200, Just "\"bar\""))

      res <-
        runError
          . mock @Service @IO
          . assertNoError @ServerError
          . assertNoError @DiscoveryFailure
          . TinyLog.discardLogs
          . mockDiscoveryTrivial
          . runInputConst noClientCertSettings
          $ callInward request

      actualCalls <- mockServiceCallCalls @IO
      embed $ assertEqual "no calls to services should be made" [] actualCalls
      embed $ void res @?= Left NoClientCertificate

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
            "/federation/brig/stuff?key=value" -- queries not allowed
          ]

    for_ invalidPaths $ \invalidPath -> do
      request <-
        exampleRequest
          "test/resources/unit/localhost.example.com.pem"
          invalidPath

      runM . evalMock @Service @IO $ do
        mockServiceCallReturns @IO (\_ _ _ _ -> pure (HTTP.ok200, Just "\"bar\""))

        res <-
          runError @ServerError
            . mock @Service @IO
            . assertNoError @ValidationError
            . assertNoError @DiscoveryFailure
            . TinyLog.discardLogs
            . mockDiscoveryTrivial
            . runInputConst noClientCertSettings
            $ callInward request

        embed $ assertEqual ("Expected request with path \"" <> cs invalidPath <> "\" to fail") (Left InvalidRoute) (void res)

        actualCalls <- mockServiceCallCalls @IO
        embed $ assertEqual "no calls to any service should be made" [] actualCalls

testPaths :: TestTree
testPaths =
  testCase "forwards requests with to services" $ do
    let paths =
          [ -- federator does not percent decode
            ("/federation/brig/%2Fi%2Fusers", "/federation/%2Fi%2Fusers"),
            -- This is an invalid URL, because it contains an invalid character.
            -- warp and wai accept it anyway. The ExternalServer percent encodes
            -- the RPC name before passing it to services.
            ("/federation/brig/stuff#fragment", "/federation/stuff%23fragment")
          ]
    for_ paths $ \(federationPath, expectedServicePath) -> do
      request <-
        exampleRequest
          "test/resources/unit/localhost.example.com.pem"
          federationPath

      runM . evalMock @Service @IO $ do
        mockServiceCallReturns @IO (\_ _ _ _ -> pure (HTTP.ok200, Just "\"bar\""))

        _res <-
          mock @Service @IO
            . assertNoError @ValidationError
            . assertNoError @DiscoveryFailure
            . assertNoError @ServerError
            . TinyLog.discardLogs
            . mockDiscoveryTrivial
            . runInputConst noClientCertSettings
            $ callInward request

        actualCalls <- mockServiceCallCalls @IO
        embed $ assertEqual "one call with expected path" [(Brig, expectedServicePath, "\"foo\"", aValidDomain)] actualCalls

testInvalidComponent :: TestTree
testInvalidComponent =
  testCase "a path with an invalid component should result in an error" $ do
    request <-
      exampleRequest
        "test/resources/unit/localhost.example.com.pem"
        "/federation/mast/get-users"

    runM . evalMock @Service @IO $ do
      mockServiceCallReturns @IO (\_ _ _ _ -> pure (HTTP.ok200, Just "\"bar\""))

      res <-
        runError @ServerError
          . mock @Service @IO
          . assertNoError @ValidationError
          . assertNoError @DiscoveryFailure
          . TinyLog.discardLogs
          . mockDiscoveryTrivial
          . runInputConst noClientCertSettings
          $ callInward request

      embed $ void res @?= Left (UnknownComponent "mast")
      actualCalls <- mockServiceCallCalls @IO
      embed $ assertEqual "no calls to any service should be made" [] actualCalls

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
          . interpret @Service (\_ -> embed $ assertFailure "unexpected call to service")
          . assertNoError @ValidationError
          . assertNoError @DiscoveryFailure
          . TinyLog.discardLogs
          . mockDiscoveryTrivial
          . runInputConst noClientCertSettings
          $ callInward request
      void res @?= Left InvalidRoute

exampleDomain :: Text
exampleDomain = "localhost.example.com"

aValidDomain :: Domain
aValidDomain = Domain exampleDomain

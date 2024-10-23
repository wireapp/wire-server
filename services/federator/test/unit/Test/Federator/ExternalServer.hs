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

module Test.Federator.ExternalServer where

import Control.Monad.Except
import Data.ByteString qualified as BS
import Data.Default
import Data.Domain
import Data.Sequence as Seq
import Data.String.Conversions
import Data.Text.Encoding qualified as Text
import Federator.Discovery
import Federator.Error.ServerError (ServerError (..))
import Federator.ExternalServer
import Federator.Interpreter
import Federator.Metrics
import Federator.Options
import Federator.Service (Service (..), ServiceStreaming)
import Federator.Validation
import Imports
import Network.HTTP.Types
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Internal qualified as Wai
import Network.Wai.Utilities.Server qualified as Wai
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Output
import Polysemy.TinyLog
import Servant.Client.Core qualified as Servant
import Servant.Server qualified as Servant
import Servant.Server.Generic
import Servant.Types.SourceT
import System.Logger (Msg)
import Test.Federator.Options (noClientCertSettings)
import Test.Federator.Util
import Test.Federator.Validation (mockDiscoveryTrivial)
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Federation.Component
import Wire.API.Routes.FederationDomainConfig
import Wire.Sem.Logger
import Wire.Sem.Logger.TinyLog

tests :: TestTree
tests =
  testGroup
    "ExternalServer"
    [ requestBrigSuccess,
      requestBrigFailure,
      requestGalleySuccess,
      requestNoCertificate,
      requestInvalidCertificate,
      requestNoDomain,
      testInvalidPaths,
      testMethod
    ]

interpretMetricsEmpty :: Sem (Metrics ': r) a -> Sem r a
interpretMetricsEmpty = interpret $ \case
  OutgoingCounterIncr _ -> pure ()
  IncomingCounterIncr _ -> pure ()

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
    cHeaders :: RequestHeaders,
    cBody :: LByteString,
    cDomain :: Domain
  }
  deriving (Eq, Show)

mockService ::
  (Members [Output Call, Embed IO] r) =>
  HTTP.Status ->
  Sem (ServiceStreaming ': r) a ->
  Sem r a
mockService status = interpret $ \case
  ServiceCall comp path headers body domain -> do
    output (Call comp path headers body domain)
    pure
      Servant.Response
        { Servant.responseStatusCode = status,
          Servant.responseHeaders = Seq.fromList headers,
          Servant.responseHttpVersion = HTTP.http11,
          Servant.responseBody = source ["\"bar\""]
        }

requestBrigSuccess :: TestTree
requestBrigSuccess =
  testCase "should forward response from brig when status is 200" $ do
    request0 <-
      exampleRequest
        "test/resources/unit/localhost.example.com.pem"
        "/federation/brig/get-user-by-handle"
    let request =
          request0
            { Wai.requestHeaders =
                ("Invalid-Header", "foo")
                  : ("X-Wire-API-Version", "v0")
                  : Wai.requestHeaders request0
            }
    Right cert <- decodeCertificate <$> BS.readFile "test/resources/unit/localhost.example.com.pem"

    let assertMetrics :: (Member (Embed IO) r) => Sem (Metrics ': r) a -> Sem r a
        assertMetrics = interpret $ \case
          OutgoingCounterIncr _ -> embed @IO $ assertFailure "Should not increment outgoing counter"
          IncomingCounterIncr od -> embed @IO $ od @?= aValidDomain

    resRef <- newIORef Nothing
    (actualCalls, _) <-
      runM
        . assertMetrics
        . runOutputList
        . mockService HTTP.ok200
        . assertNoError @ValidationError
        . assertNoError @DiscoveryFailure
        . assertNoError @ServerError
        . discardTinyLogs
        . mockDiscoveryTrivial
        . runInputConst noClientCertSettings
        . runInputConst scaffoldingFederationDomainConfigs
        $ callInward Brig (RPC "get-user-by-handle") aValidDomain (CertHeader cert) request (saveResponse resRef)

    Just res <- readIORef resRef
    let expectedCall = Call Brig "/federation/get-user-by-handle" [("X-Wire-API-Version", "v0")] "\"foo\"" aValidDomain
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
    Right cert <- decodeCertificate <$> BS.readFile "test/resources/unit/localhost.example.com.pem"

    resRef <- newIORef Nothing
    (actualCalls, _) <-
      runM
        . interpretMetricsEmpty
        . runOutputList
        . mockService HTTP.notFound404
        . assertNoError @ValidationError
        . assertNoError @DiscoveryFailure
        . assertNoError @ServerError
        . discardTinyLogs
        . mockDiscoveryTrivial
        . runInputConst noClientCertSettings
        . runInputConst scaffoldingFederationDomainConfigs
        $ callInward Brig (RPC "get-user-by-handle") aValidDomain (CertHeader cert) request (saveResponse resRef)

    Just res <- readIORef resRef
    let expectedCall = Call Brig "/federation/get-user-by-handle" [] "\"foo\"" aValidDomain
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

    Right cert <- decodeCertificate <$> BS.readFile "test/resources/unit/localhost.example.com.pem"

    resRef <- newIORef Nothing
    (actualCalls, _) <-
      runM
        . runOutputList
        . interpretMetricsEmpty
        . mockService HTTP.ok200
        . assertNoError @ValidationError
        . assertNoError @DiscoveryFailure
        . assertNoError @ServerError
        . discardTinyLogs
        . mockDiscoveryTrivial
        . runInputConst noClientCertSettings
        . runInputConst scaffoldingFederationDomainConfigs
        $ callInward Galley (RPC "get-conversations") aValidDomain (CertHeader cert) request (saveResponse resRef)

    Just res <- readIORef resRef
    let expectedCall = Call Galley "/federation/get-conversations" [] "\"foo\"" aValidDomain
    assertEqual "one call to galley should be made" [expectedCall] actualCalls
    Wai.responseStatus res @?= HTTP.status200
    body <- Wai.lazyResponseBody res
    body @?= "\"bar\""

requestNoDomain :: TestTree
requestNoDomain =
  testCase "should fail with a 404 when no origin domain header is given" $ do
    cert <- BS.readFile "test/resources/unit/localhost.example.com.pem"
    request <-
      testRequest
        def
          { trCertificateHeader = Just cert,
            trPath = "/federation/brig/get-users"
          }
    serviceCallsRef <- newIORef []
    let serverApp = genericServeT (testInterpreter serviceCallsRef) $ server undefined undefined
    void . serverApp request $ \res -> do
      serviceCalls <- readIORef serviceCallsRef
      assertEqual "Expected response to have status 400" status400 (Wai.responseStatus res)
      assertEqual "no calls to any service should be made" [] serviceCalls
      pure Wai.ResponseReceived

requestNoCertificate :: TestTree
requestNoCertificate =
  testCase "should fail with a 404 when no certificate is given" $ do
    request <-
      testRequest
        def
          { trDomainHeader = Just (Text.encodeUtf8 exampleDomain),
            trPath = "/federation/brig/get-users"
          }
    serviceCallsRef <- newIORef []
    let serverApp = genericServeT (testInterpreter serviceCallsRef) $ server undefined undefined
    void . serverApp request $ \res -> do
      serviceCalls <- readIORef serviceCallsRef
      assertEqual "Expected response to have status 400" status400 (Wai.responseStatus res)
      assertEqual "no calls to any service should be made" [] serviceCalls
      pure Wai.ResponseReceived

-- @SF.Federation @TSFI.Federate @TSFI.DNS @S2 @S3 @S7
-- Reject request if the client certificate for federator is invalid
requestInvalidCertificate :: TestTree
requestInvalidCertificate =
  testCase "testRequestInvalidCertificate - should fail with a 404 when an invalid certificate is given" $ do
    request <-
      testRequest
        def
          { trDomainHeader = Just (Text.encodeUtf8 exampleDomain),
            trPath = "/federation/brig/get-users",
            trCertificateHeader = Just "not a certificate"
          }
    serviceCallsRef <- newIORef []
    let serverApp = genericServeT (testInterpreter serviceCallsRef) $ server undefined undefined
    void . serverApp request $ \res -> do
      serviceCalls <- readIORef serviceCallsRef
      assertEqual "Expected response to have status 400" status400 (Wai.responseStatus res)
      assertEqual "no calls to any service should be made" [] serviceCalls
      pure Wai.ResponseReceived

-- @END

testInvalidPaths :: TestTree
testInvalidPaths = do
  let invalidPaths =
        [ ("", status404),
          ("/", status404),
          ("///", status404),
          -- disallowed paths
          ("federation", status404),
          ("/federation", status404),
          ("/federation/", status404),
          ("/federation/brig", status404),
          ("/federation/brig/", status404), -- empty component
          ("i/users", status404),
          ("/i/users", status404),
          ("/federation/brig/too/many/components", status404),
          -- syntax we don't wish to support
          ("http://federation.wire.link/federation/galley", status404), -- contains scheme and domain
          ("http://federation/stuff", status404), -- contains scheme
          ("federation.wire.link/federation/brig/stuff", status404), -- contains domain
          ("/federation/brig/rpc?bar[]=baz", status403), -- queries not allowed
          ("/federation/brig/stuff?key=value", status403), -- queries not allowed
          -- rpc names that don't match [0-9a-zA-Z-_]+
          ("/federation/brig/../i/users", status404),
          ("/federation/brig/%2e%2e/i/users", status404), -- percent-encoded '../'
          ("/federation/brig/%2E%2E/i/users", status404),
          ("/federation/brig/..%2Fi%2Fusers", status400), -- percent-encoded ../i/users
          ("/federation/brig/%252e%252e/i/users", status404), -- double percent-encoded '../'
          ("/federation/brig/%c0%ae%c0%ae/i/users", status404), -- weird-encoded '../'
          ("/federation/mast/get-users", status400) -- invalid component
        ]
  testGroup "should not forward requests with invalid paths to services" $
    map invalidPathTest invalidPaths
  where
    invalidPathTest :: (ByteString, Status) -> TestTree
    invalidPathTest (invalidPath, expectedStatus) =
      testCase (cs invalidPath) $ do
        request <-
          exampleRequest
            "test/resources/unit/localhost.example.com.pem"
            invalidPath

        serviceCallsRef <- newIORef []
        let serverApp = genericServeT (testInterpreter serviceCallsRef) $ server undefined undefined
        void . serverApp request $ \res -> do
          serviceCalls <- readIORef serviceCallsRef
          assertEqual "Unexpected status" expectedStatus (Wai.responseStatus res)
          assertEqual "no calls to any service should be made" [] serviceCalls
          pure Wai.ResponseReceived

testMethod :: TestTree
testMethod =
  testGroup "only POST should be supported" $
    let invalidMethodTest method = testCase (cs method) $ do
          cert <- BS.readFile "test/resources/unit/localhost.example.com.pem"
          let tr =
                def
                  { trPath = "/federation/galley/send-message",
                    trDomainHeader = Just (Text.encodeUtf8 exampleDomain),
                    trCertificateHeader = Just cert,
                    trBody = "\"hello\""
                  }
          request <- testRequest tr {trMethod = method}
          serviceCallsRef <- newIORef []
          let serverApp = genericServeT (testInterpreter serviceCallsRef) $ server undefined undefined
          void . serverApp request $ \res -> do
            serviceCalls <- readIORef serviceCallsRef
            assertEqual "Expected response to have status 403" status403 (Wai.responseStatus res)
            assertEqual "no calls to any service should be made" [] serviceCalls
            pure Wai.ResponseReceived
     in map invalidMethodTest [HTTP.methodGet, HTTP.methodDelete, HTTP.methodPut, HTTP.methodPatch]

testInterpreter ::
  IORef [Call] ->
  Sem
    '[ Metrics,
       Input FederationDomainConfigs,
       Input RunSettings,
       DiscoverFederator,
       Error DiscoveryFailure,
       Error ValidationError,
       Error ServerError,
       Error Servant.ServerError,
       Logger (Msg -> Msg),
       ServiceStreaming,
       Output Call,
       Embed IO
     ]
    a ->
  Servant.Handler a
testInterpreter serviceCallsRef =
  Servant.Handler
    . ExceptT
    . runM @IO
    . runOutputMonoidIORef @Call serviceCallsRef (: [])
    . mockService HTTP.ok200
    . discardLogs
    . runError
    . runWaiErrors @'[DiscoveryFailure, ValidationError, ServerError]
    . mockDiscoveryTrivial
    . runInputConst noClientCertSettings
    . runInputConst scaffoldingFederationDomainConfigs
    . interpretMetricsEmpty

saveResponse :: IORef (Maybe Wai.Response) -> Wai.Response -> IO Wai.ResponseReceived
saveResponse ref res = writeIORef ref (Just res) $> Wai.ResponseReceived

exampleDomain :: Text
exampleDomain = "localhost.example.com"

aValidDomain :: Domain
aValidDomain = Domain exampleDomain

scaffoldingFederationDomainConfigs :: FederationDomainConfigs
scaffoldingFederationDomainConfigs = defFederationDomainConfigs {strategy = AllowAll}

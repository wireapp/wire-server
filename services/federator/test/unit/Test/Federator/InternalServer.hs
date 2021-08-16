{-# OPTIONS_GHC -Wno-orphans #-}

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

module Test.Federator.InternalServer (tests) where

import Data.Domain (Domain (Domain))
import Federator.Discovery (LookupError (LookupErrorDNSError, LookupErrorSrvNotAvailable))
import Federator.InternalServer (callOutward)
import Federator.Options (AllowedDomains (..), FederationStrategy (..), RunSettings (..))
import Federator.Remote (Remote, RemoteError (RemoteErrorDiscoveryFailure))
import Imports
import Mu.GRpc.Client.Record
import Network.HTTP2.Client (TooMuchConcurrency (TooMuchConcurrency))
import Polysemy (embed, runM)
import qualified Polysemy.Reader as Polysemy
import Test.Federator.Options (noClientCertSettings)
import Test.Polysemy.Mock (Mock (mock), evalMock)
import Test.Polysemy.Mock.TH (genMock)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Wire.API.Federation.GRPC.Types

genMock ''Remote

tests :: TestTree
tests =
  testGroup "Federate" $
    [ testGroup "with remote" $
        [ federatedRequestSuccess,
          federatedRequestFailureTMC,
          federatedRequestFailureErrCode,
          federatedRequestFailureErrStr,
          federatedRequestFailureNoRemote,
          federatedRequestFailureDNS,
          federatedRequestFailureAllowList
        ]
    ]

settingsWithAllowList :: [Domain] -> RunSettings
settingsWithAllowList domains =
  noClientCertSettings {federationStrategy = AllowList (AllowedDomains domains)}

federatedRequestSuccess :: TestTree
federatedRequestSuccess =
  testCase "should successfully return success response" $
    runM . evalMock @Remote @IO $ do
      mockDiscoverAndCallReturns @IO (const $ pure (Right (GRpcOk (InwardResponseBody "success!"))))
      let federatedRequest = FederatedRequest validDomainText (Just validLocalPart)

      res <-
        mock @Remote @IO . Polysemy.runReader noClientCertSettings $
          callOutward federatedRequest

      actualCalls <- mockDiscoverAndCallCalls @IO
      let expectedCall = ValidatedFederatedRequest (Domain validDomainText) validLocalPart
      embed $ assertEqual "one remote call should be made" [expectedCall] actualCalls
      embed $ assertEqual "successful response should be returned" (OutwardResponseBody "success!") res

-- FUTUREWORK(federation): This is probably not ideal, we should figure out what this error
-- means and act accordingly.
federatedRequestFailureTMC :: TestTree
federatedRequestFailureTMC =
  testCase "should respond with error when facing GRpcTooMuchConcurrency" $
    runM . evalMock @Remote @IO $ do
      mockDiscoverAndCallReturns @IO (const $ pure (Right (GRpcTooMuchConcurrency (TooMuchConcurrency 2))))
      let federatedRequest = FederatedRequest validDomainText (Just validLocalPart)

      res <- mock @Remote @IO . Polysemy.runReader noClientCertSettings $ callOutward federatedRequest

      actualCalls <- mockDiscoverAndCallCalls @IO
      let expectedCall = ValidatedFederatedRequest (Domain validDomainText) validLocalPart
      embed $ do
        assertEqual "one remote call should be made" [expectedCall] actualCalls
        assertResponseErrorWithType RemoteFederatorError res

federatedRequestFailureErrCode :: TestTree
federatedRequestFailureErrCode =
  testCase "should respond with error when facing GRpcErrorCode" $
    runM . evalMock @Remote @IO $ do
      mockDiscoverAndCallReturns @IO (const $ pure (Right (GRpcErrorCode 77))) -- TODO: Maybe use some legit HTTP2 error code?
      let federatedRequest = FederatedRequest validDomainText (Just validLocalPart)

      res <- mock @Remote @IO . Polysemy.runReader noClientCertSettings $ callOutward federatedRequest

      actualCalls <- mockDiscoverAndCallCalls @IO
      let expectedCall = ValidatedFederatedRequest (Domain validDomainText) validLocalPart
      embed $ do
        assertEqual "one remote call should be made" [expectedCall] actualCalls
        assertResponseErrorWithType RemoteFederatorError res

federatedRequestFailureErrStr :: TestTree
federatedRequestFailureErrStr =
  testCase "should respond with error when facing GRpcErrorString" $
    runM . evalMock @Remote @IO $ do
      mockDiscoverAndCallReturns @IO (const $ pure (Right (GRpcErrorString "some grpc error")))
      let federatedRequest = FederatedRequest validDomainText (Just validLocalPart)

      res <- mock @Remote @IO . Polysemy.runReader noClientCertSettings $ callOutward federatedRequest

      actualCalls <- mockDiscoverAndCallCalls @IO
      let expectedCall = ValidatedFederatedRequest (Domain validDomainText) validLocalPart
      embed $ do
        assertEqual "one remote call should be made" [expectedCall] actualCalls
        assertResponseErrorWithType RemoteFederatorError res

federatedRequestFailureNoRemote :: TestTree
federatedRequestFailureNoRemote =
  testCase "should respond with error when SRV record is not found" $
    runM . evalMock @Remote @IO $ do
      mockDiscoverAndCallReturns @IO (const $ pure (Left $ RemoteErrorDiscoveryFailure (Domain "example.com") (LookupErrorSrvNotAvailable "_something._tcp.example.com")))
      let federatedRequest = FederatedRequest validDomainText (Just validLocalPart)

      res <- mock @Remote @IO . Polysemy.runReader noClientCertSettings $ callOutward federatedRequest

      actualCalls <- mockDiscoverAndCallCalls @IO
      let expectedCall = ValidatedFederatedRequest (Domain validDomainText) validLocalPart
      embed $ do
        assertEqual "one remote call should be made" [expectedCall] actualCalls
        assertResponseErrorWithType RemoteNotFound res

federatedRequestFailureDNS :: TestTree
federatedRequestFailureDNS =
  testCase "should respond with error when SRV lookup fails due to DNSError" $
    runM . evalMock @Remote @IO $ do
      mockDiscoverAndCallReturns @IO (const $ pure (Left $ RemoteErrorDiscoveryFailure (Domain "example.com") (LookupErrorDNSError "No route to 1.1.1.1")))
      let federatedRequest = FederatedRequest validDomainText (Just validLocalPart)

      res <- mock @Remote @IO . Polysemy.runReader noClientCertSettings $ callOutward federatedRequest

      actualCalls <- mockDiscoverAndCallCalls @IO
      let expectedCall = ValidatedFederatedRequest (Domain validDomainText) validLocalPart
      embed $ do
        assertEqual "one remote call should be made" [expectedCall] actualCalls
        assertResponseErrorWithType DiscoveryFailed res

federatedRequestFailureAllowList :: TestTree
federatedRequestFailureAllowList =
  testCase "should not make a call when target domain not in the allowList" $
    runM . evalMock @Remote @IO $ do
      let federatedRequest = FederatedRequest validDomainText (Just validLocalPart)
      let settings = settingsWithAllowList [Domain "hello.world"]
      res <- mock @Remote @IO . Polysemy.runReader settings $ callOutward federatedRequest

      actualCalls <- mockDiscoverAndCallCalls @IO
      embed $ do
        assertEqual "no remote calls should be made" [] actualCalls
        assertResponseErrorWithType FederationDeniedLocally res

assertResponseErrorWithType :: HasCallStack => OutwardErrorType -> OutwardResponse -> IO ()
assertResponseErrorWithType expectedType res =
  case res of
    OutwardResponseBody _ ->
      assertFailure $ "Expected response to be error, but it was not: " <> show res
    OutwardResponseInwardError err ->
      assertFailure $ "Expected response to be outward error, but it was not: " <> show err
    OutwardResponseError (OutwardError actualType _) ->
      assertEqual "Unexpected error type" expectedType actualType

validLocalPart :: Request
validLocalPart = Request Brig "/users" "\"foo\"" "foo.domain"

validDomainText :: Text
validDomainText = "example.com"

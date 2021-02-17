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

module Test.Federator.InternalServer where

import Data.Domain (Domain (Domain))
import Data.Either.Validation
import Federator.Discovery (LookupError (LookupErrorSrvNotAvailable))
import Federator.InternalServer (callOutward)
import Federator.Options (AllowedDomains (..), FederationStrategy (..), RunSettings (..))
import Federator.Remote (Remote, RemoteError (RemoteErrorDiscoveryFailure))
import Federator.Util
import Imports
import Mu.GRpc.Client.Record
import qualified Network.HTTP.Types as HTTP
import Network.HTTP2.Client (TooMuchConcurrency (TooMuchConcurrency))
import Polysemy (embed, runM)
import qualified Polysemy.Reader as Polysemy
import Test.Polysemy.Mock (Mock (mock), evalMock)
import Test.Polysemy.Mock.TH (genMock)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Wire.API.Federation.GRPC.Types

genMock ''Remote

tests :: TestTree
tests =
  testGroup "Fedderate" $
    [ testGroup "Util.federateWith" $
        [ federateWithAllowListSuccess,
          federateWithAllowListFail
        ],
      testGroup "with remote" $
        [ federatedRequestSuccess,
          federatedRequestFailureTMC,
          federatedRequestFailureErrCode,
          federatedRequestFailureErrStr,
          federatedRequestFailureErrConn,
          federatedRequestFailureAllowList
        ]
    ]

allowAllSettings :: RunSettings
allowAllSettings = RunSettings AllowAll

federatedRequestSuccess :: TestTree
federatedRequestSuccess =
  testCase "should successfully return success response" $
    runM . evalMock @Remote @IO $ do
      mockDiscoverAndCallReturns @IO (const $ pure (Right (GRpcOk (ResponseHTTPResponse (HTTPResponse 200 "success!")))))
      let federatedRequest = FederatedRequest validDomainText (Just validLocalPart)

      res <- mock @Remote @IO . Polysemy.runReader allowAllSettings $ callOutward federatedRequest

      actualCalls <- mockDiscoverAndCallCalls @IO
      let expectedCall = ValidatedFederatedRequest (Domain validDomainText) validLocalPart
      embed $ assertEqual "one remote call should be made" [expectedCall] actualCalls
      embed $ assertEqual "successful response should be returned" (ResponseHTTPResponse (HTTPResponse 200 "success!")) res

-- FUTUREWORK(federation): This is probably not ideal, we should figure out what this error
-- means and act accordingly.
federatedRequestFailureTMC :: TestTree
federatedRequestFailureTMC =
  testCase "should respond with error when facing GRpcTooMuchConcurrency" $
    runM . evalMock @Remote @IO $ do
      mockDiscoverAndCallReturns @IO (const $ pure (Right (GRpcTooMuchConcurrency (TooMuchConcurrency 2))))
      let federatedRequest = FederatedRequest validDomainText (Just validLocalPart)

      res <- mock @Remote @IO . Polysemy.runReader allowAllSettings $ callOutward federatedRequest

      actualCalls <- mockDiscoverAndCallCalls @IO
      let expectedCall = ValidatedFederatedRequest (Domain validDomainText) validLocalPart
      embed $ assertEqual "one remote call should be made" [expectedCall] actualCalls
      embed $ assertBool "the response should be error" (isResponseError res)

federatedRequestFailureErrCode :: TestTree
federatedRequestFailureErrCode =
  testCase "should respond with error when facing GRpcErrorCode" $
    runM . evalMock @Remote @IO $ do
      mockDiscoverAndCallReturns @IO (const $ pure (Right (GRpcErrorCode 77))) -- TODO: Maybe use some legit HTTP2 error code?
      let federatedRequest = FederatedRequest validDomainText (Just validLocalPart)

      res <- mock @Remote @IO . Polysemy.runReader allowAllSettings $ callOutward federatedRequest

      actualCalls <- mockDiscoverAndCallCalls @IO
      let expectedCall = ValidatedFederatedRequest (Domain validDomainText) validLocalPart
      embed $ assertEqual "one remote call should be made" [expectedCall] actualCalls
      embed $ assertBool "the response should be error" (isResponseError res)

federatedRequestFailureErrStr :: TestTree
federatedRequestFailureErrStr =
  testCase "should respond with error when facing GRpcErrorString" $
    runM . evalMock @Remote @IO $ do
      mockDiscoverAndCallReturns @IO (const $ pure (Right (GRpcErrorString "some grpc error")))
      let federatedRequest = FederatedRequest validDomainText (Just validLocalPart)

      res <- mock @Remote @IO . Polysemy.runReader allowAllSettings $ callOutward federatedRequest

      actualCalls <- mockDiscoverAndCallCalls @IO
      let expectedCall = ValidatedFederatedRequest (Domain validDomainText) validLocalPart
      embed $ assertEqual "one remote call should be made" [expectedCall] actualCalls
      embed $ assertBool "the response should have error" (isResponseError res)

federatedRequestFailureErrConn :: TestTree
federatedRequestFailureErrConn =
  testCase "should respond with error when facing RemoteError" $
    runM . evalMock @Remote @IO $ do
      mockDiscoverAndCallReturns @IO (const $ pure (Left $ RemoteErrorDiscoveryFailure (LookupErrorSrvNotAvailable "_something._tcp.example.com") (Domain "example.com")))
      let federatedRequest = FederatedRequest validDomainText (Just validLocalPart)

      res <- mock @Remote @IO . Polysemy.runReader allowAllSettings $ callOutward federatedRequest

      actualCalls <- mockDiscoverAndCallCalls @IO
      let expectedCall = ValidatedFederatedRequest (Domain validDomainText) validLocalPart
      embed $ assertEqual "one remote call should be made" [expectedCall] actualCalls
      embed $ assertBool "the response should have error" (isResponseError res)

federatedRequestFailureAllowList :: TestTree
federatedRequestFailureAllowList =
  testCase "should not make a call when target domain not in the allowList" $
    runM . evalMock @Remote @IO $ do
      mockDiscoverAndCallReturns @IO (const $ pure (Left $ RemoteErrorDiscoveryFailure (LookupErrorSrvNotAvailable "_something._tcp.example.com") (Domain "example.com")))
      let federatedRequest = FederatedRequest validDomainText (Just validLocalPart)

      let allowList = RunSettings (AllowList (AllowedDomains [Domain "hello.world"]))

      res <- mock @Remote @IO . Polysemy.runReader allowList $ callOutward federatedRequest

      actualCalls <- mockDiscoverAndCallCalls @IO
      embed $ assertEqual "no remote calls should be made" [] actualCalls
      embed $ assertBool "the response should have error" (isResponseError res)

federateWithAllowListSuccess :: TestTree
federateWithAllowListSuccess =
  testCase "should give True when target domain is in the list" $
    runM . evalMock @Remote @IO $ do
      let allowList = RunSettings (AllowList (AllowedDomains [Domain "hello.world"]))
      res <- Polysemy.runReader allowList $ federateWith (Domain "hello.world")
      embed $ assertBool "federating should be allowed" res

federateWithAllowListFail :: TestTree
federateWithAllowListFail =
  testCase "should give False when target domain is not in the list" $
    runM . evalMock @Remote @IO $ do
      let allowList = RunSettings (AllowList (AllowedDomains [Domain "only.other.domain"]))
      res <- Polysemy.runReader allowList $ federateWith (Domain "hello.world")
      embed $ assertBool "federating should not be allowed" (not res)

isResponseError :: Response -> Bool
isResponseError (ResponseErr _) = True
isResponseError (ResponseHTTPResponse _) = False

isRight' :: Validation a b -> Bool
isRight' = isRight . validationToEither

validLocalPart :: Request
validLocalPart = Request Brig (HTTPMethod HTTP.GET) "/users" [QueryParam "handle" "foo"] mempty

validDomainText :: Text
validDomainText = "example.com"

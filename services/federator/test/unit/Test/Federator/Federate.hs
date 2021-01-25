{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Federator.Federate where

import Data.Domain (Domain (Domain))
import Data.Either.Validation
import Federator.Federate
import Imports
import Mu.GRpc.Client.Record
import qualified Network.HTTP.Types as HTTP
import Network.HTTP2.Client (TooMuchConcurrency (TooMuchConcurrency))
import Polysemy (embed, runM)
import Test.Polysemy.Mock (Mock (mock), evalMock)
import Test.Polysemy.Mock.TH (genMock)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Wire.API.Federation.GRPC.Types

genMock ''Remote
genMock ''Brig

tests :: TestTree
tests =
  testGroup "Fedderate" $
    [ testGroup "with remote" $
        [ remoteCallSuccess,
          remoteCallFailureTMC,
          remoteCallFailureErrCode,
          remoteCallFailureErrStr,
          remoteCallFailureErrConn
        ],
      testGroup "with local" $
        [ localCallBrigSuccess
        ]
    ]

remoteCallSuccess :: TestTree
remoteCallSuccess =
  testCase "should successfully return success response" $
    runM . evalMock @Remote @IO $ do
      mockDiscoverAndCallReturns @IO (const $ pure (Right (GRpcOk (ResponseOk "success!"))))
      let remoteCall = RemoteCall validDomainText (Just $ validatedLocalCallToLocalCall validLocalPart)

      res <- mock @Remote @IO $ callRemote remoteCall

      actualCalls <- mockDiscoverAndCallCalls @IO
      let expectedCall = ValidatedRemoteCall (Domain validDomainText) validLocalPart
      embed $ assertEqual "one remote call should be made" [expectedCall] actualCalls
      embed $ assertEqual "successful response should be returned" (ResponseOk "success!") res

-- FUTUREWORK: This is probably not ideal, we should figure out what this error
-- means and act accordingly.
remoteCallFailureTMC :: TestTree
remoteCallFailureTMC =
  testCase "should respond with error when facing GRpcTooMuchConcurrency" $
    runM . evalMock @Remote @IO $ do
      mockDiscoverAndCallReturns @IO (const $ pure (Right (GRpcTooMuchConcurrency (TooMuchConcurrency 2))))
      let remoteCall = RemoteCall validDomainText (Just $ validatedLocalCallToLocalCall validLocalPart)

      res <- mock @Remote @IO $ callRemote remoteCall

      actualCalls <- mockDiscoverAndCallCalls @IO
      let expectedCall = ValidatedRemoteCall (Domain validDomainText) validLocalPart
      embed $ assertEqual "one remote call should be made" [expectedCall] actualCalls
      embed $ assertBool "the response should be error" (isResponseError res)

remoteCallFailureErrCode :: TestTree
remoteCallFailureErrCode =
  testCase "should respond with error when facing GRpcErrorCode" $
    runM . evalMock @Remote @IO $ do
      mockDiscoverAndCallReturns @IO (const $ pure (Right (GRpcErrorCode 77))) -- TODO: Maybe use some legit HTTP2 error code?
      let remoteCall = RemoteCall validDomainText (Just $ validatedLocalCallToLocalCall validLocalPart)

      res <- mock @Remote @IO $ callRemote remoteCall

      actualCalls <- mockDiscoverAndCallCalls @IO
      let expectedCall = ValidatedRemoteCall (Domain validDomainText) validLocalPart
      embed $ assertEqual "one remote call should be made" [expectedCall] actualCalls
      embed $ assertBool "the response should be error" (isResponseError res)

remoteCallFailureErrStr :: TestTree
remoteCallFailureErrStr =
  testCase "should respond with error when facing GRpcErrorString" $
    runM . evalMock @Remote @IO $ do
      mockDiscoverAndCallReturns @IO (const $ pure (Right (GRpcErrorString "some grpc error")))
      let remoteCall = RemoteCall validDomainText (Just $ validatedLocalCallToLocalCall validLocalPart)

      res <- mock @Remote @IO $ callRemote remoteCall

      actualCalls <- mockDiscoverAndCallCalls @IO
      let expectedCall = ValidatedRemoteCall (Domain validDomainText) validLocalPart
      embed $ assertEqual "one remote call should be made" [expectedCall] actualCalls
      embed $ assertBool "the response should have error" (isResponseError res)

remoteCallFailureErrConn :: TestTree
remoteCallFailureErrConn =
  testCase "should respond with error when facing RemoteError" $
    runM . evalMock @Remote @IO $ do
      mockDiscoverAndCallReturns @IO (const $ pure (Left $ RemoteErrorDiscoveryFailure (LookupErrorSrvNotAvailable "_something._tcp.exmaple.com") (Domain "example.com")))
      let remoteCall = RemoteCall validDomainText (Just $ validatedLocalCallToLocalCall validLocalPart)

      res <- mock @Remote @IO $ callRemote remoteCall

      actualCalls <- mockDiscoverAndCallCalls @IO
      let expectedCall = ValidatedRemoteCall (Domain validDomainText) validLocalPart
      embed $ assertEqual "one remote call should be made" [expectedCall] actualCalls
      embed $ assertBool "the response should have error" (isResponseError res)

-- TODO: Add more tests
localCallBrigSuccess :: TestTree
localCallBrigSuccess =
  testCase "should sucessfully return on HTTP 200" $
    runM . evalMock @Brig @IO $ do
      mockBrigCallReturns @IO (\_ _ _ _ -> pure (HTTP.status200, Just "response body"))
      let request = LocalCall (Just Brig) (Just $ HTTPMethod HTTP.GET) "/users" [QueryParam "handle" "foo"] mempty

      res <- mock @Brig @IO $ callLocal request

      actualCalls <- mockBrigCallCalls @IO
      let expectedCall = (HTTP.GET, "/users", [QueryParam "handle" "foo"], mempty)
      embed $ assertEqual "one call to brig should be made" [expectedCall] actualCalls
      embed $ assertEqual "response should be success with correct body" (ResponseOk "response body") res

isResponseError :: Response -> Bool
isResponseError (ResponseErr _) = True
isResponseError (ResponseOk _) = False

isRight' :: Validation a b -> Bool
isRight' = isRight . validationToEither

validLocalPart :: ValidatedLocalCall
validLocalPart = ValidatedLocalCall Brig HTTP.GET "/users" [QueryParam "handle" "foo"] mempty

validDomainText :: Text
validDomainText = "example.com"

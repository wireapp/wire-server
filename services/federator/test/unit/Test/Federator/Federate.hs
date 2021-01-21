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
import Test.Tasty.QuickCheck hiding (Failure, Success)

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
        [ localCallBrigSuccess,
          testGroup "validateLocalCall" $
            [ testValidateLocalCallWhenValid,
              testValidateLocalCallWhenComponentIsMissing,
              testValidateLocalCallWhenMethodIsMissing
            ]
        ]
    ]

remoteCallSuccess :: TestTree
remoteCallSuccess =
  testCase "should successfully return success response" $
    runM . evalMock @Remote @IO $ do
      mockDiscoverAndCallReturns @IO (const $ pure (Right (GRpcOk (ResponseOk "success!"))))
      let remoteCall = RemoteCall (Domain "example.com") (LocalCall (Just Brig) (Just $ HTTPMethod HTTP.GET) "/users" [QueryParam "handle" "foo"] mempty)
      res <- mock @Remote @IO $ callRemote remoteCall
      actualCalls <- mockDiscoverAndCallCalls @IO
      embed $ assertEqual "one remote call should be made" [remoteCall] actualCalls
      embed $ assertEqual "successful response should be returned" (ResponseOk "success!") res

-- FUTUREWORK: This is probably not ideal, we should figure out what this error
-- means and act accordingly.
remoteCallFailureTMC :: TestTree
remoteCallFailureTMC =
  testCase "should respond with error when facing GRpcTooMuchConcurrency" $
    runM . evalMock @Remote @IO $ do
      mockDiscoverAndCallReturns @IO (const $ pure (Right (GRpcTooMuchConcurrency (TooMuchConcurrency 2))))
      let remoteCall = RemoteCall (Domain "example.com") (LocalCall (Just Brig) (Just $ HTTPMethod HTTP.GET) "/users" [QueryParam "handle" "foo"] mempty)
      res <- mock @Remote @IO $ callRemote remoteCall
      actualCalls <- mockDiscoverAndCallCalls @IO
      embed $ assertEqual "one remote call should be made" [remoteCall] actualCalls
      embed $ assertBool "the response should be error" (isResponseError res)

remoteCallFailureErrCode :: TestTree
remoteCallFailureErrCode =
  testCase "should respond with error when facing GRpcErrorCode" $
    runM . evalMock @Remote @IO $ do
      mockDiscoverAndCallReturns @IO (const $ pure (Right (GRpcErrorCode 77))) -- TODO: Maybe use some legit HTTP2 error code?
      let remoteCall = RemoteCall (Domain "example.com") (LocalCall (Just Brig) (Just $ HTTPMethod HTTP.GET) "/users" [QueryParam "handle" "foo"] mempty)
      res <- mock @Remote @IO $ callRemote remoteCall
      actualCalls <- mockDiscoverAndCallCalls @IO
      embed $ assertEqual "one remote call should be made" [remoteCall] actualCalls
      embed $ assertBool "the response should be error" (isResponseError res)

remoteCallFailureErrStr :: TestTree
remoteCallFailureErrStr =
  testCase "should respond with error when facing GRpcErrorString" $
    runM . evalMock @Remote @IO $ do
      mockDiscoverAndCallReturns @IO (const $ pure (Right (GRpcErrorString "some grpc error")))
      let remoteCall = RemoteCall (Domain "example.com") (LocalCall (Just Brig) (Just $ HTTPMethod HTTP.GET) "/users" [QueryParam "handle" "foo"] mempty)
      res <- mock @Remote @IO $ callRemote remoteCall
      actualCalls <- mockDiscoverAndCallCalls @IO
      embed $ assertEqual "one remote call should be made" [remoteCall] actualCalls
      embed $ assertBool "the response should have error" (isResponseError res)

remoteCallFailureErrConn :: TestTree
remoteCallFailureErrConn =
  testCase "should respond with error when facing RemoteError" $
    runM . evalMock @Remote @IO $ do
      mockDiscoverAndCallReturns @IO (const $ pure (Left $ RemoteErrorDiscoveryFailure (LookupErrorSrvNotAvailable "_something._tcp.exmaple.com") (Domain "example.com")))
      let remoteCall = RemoteCall (Domain "example.com") (LocalCall (Just Brig) (Just $ HTTPMethod HTTP.GET) "/users" [QueryParam "handle" "foo"] mempty)
      res <- mock @Remote @IO $ callRemote remoteCall
      actualCalls <- mockDiscoverAndCallCalls @IO
      embed $ assertEqual "one remote call should be made" [remoteCall] actualCalls
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
      embed $ assertEqual "one call to brig should be made" [(HTTP.GET, "/users", [QueryParam "handle" "foo"], mempty)] actualCalls
      embed $ assertEqual "response should be success with correct body" (ResponseOk "response body") res

testValidateLocalCallWhenValid :: TestTree
testValidateLocalCallWhenValid =
  let validCallGen =
        arbitrary
          `suchThat` ( \LocalCall {..} ->
                         isJust component && component /= Just UnspecifiedComponent && isJust method
                     )
   in testProperty "valid calls" $ forAll validCallGen $ \c -> isRight' (validateLocalCall c)

testValidateLocalCallWhenComponentIsMissing :: TestTree
testValidateLocalCallWhenComponentIsMissing =
  let callGen = arbitrary `suchThat` (\LocalCall {..} -> component == Just UnspecifiedComponent || isNothing component)
   in testProperty "component missing" $
        forAll callGen $ \c ->
          case validateLocalCall c of
            Success _ -> False
            Failure errs -> ComponentMissing `elem` errs

testValidateLocalCallWhenMethodIsMissing :: TestTree
testValidateLocalCallWhenMethodIsMissing =
  let callGen = arbitrary `suchThat` (\LocalCall {..} -> isNothing method)
   in testProperty "method missing" $
        forAll callGen $ \c ->
          case validateLocalCall c of
            Success _ -> False
            Failure errs -> MethodMissing `elem` errs

isResponseError :: Response -> Bool
isResponseError (ResponseErr _) = True
isResponseError (ResponseOk _) = False

isRight' :: Validation a b -> Bool
isRight' = isRight . validationToEither

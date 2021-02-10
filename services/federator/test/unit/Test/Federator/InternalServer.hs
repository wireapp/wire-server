{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Federator.InternalServer where

import Data.Domain (Domain (Domain))
import Data.Either.Validation
import Federator.Discovery (LookupError (LookupErrorSrvNotAvailable))
import Federator.InternalServer (callRemote)
import Federator.Remote (Remote, RemoteError (RemoteErrorDiscoveryFailure))
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

tests :: TestTree
tests =
  testGroup "Fedderate" $
    [ testGroup "with remote" $
        [ remoteCallSuccess,
          remoteCallFailureTMC,
          remoteCallFailureErrCode,
          remoteCallFailureErrStr,
          remoteCallFailureErrConn
        ]
    ]

remoteCallSuccess :: TestTree
remoteCallSuccess =
  testCase "should successfully return success response" $
    runM . evalMock @Remote @IO $ do
      mockDiscoverAndCallReturns @IO (const $ pure (Right (GRpcOk (ResponseHTTPResponse (HTTPResponse 200 "success!")))))
      let remoteCall = RemoteCall validDomainText (Just validLocalPart)

      res <- mock @Remote @IO $ callRemote remoteCall

      actualCalls <- mockDiscoverAndCallCalls @IO
      let expectedCall = ValidatedRemoteCall (Domain validDomainText) validLocalPart
      embed $ assertEqual "one remote call should be made" [expectedCall] actualCalls
      embed $ assertEqual "successful response should be returned" (ResponseHTTPResponse (HTTPResponse 200 "success!")) res

-- FUTUREWORK: This is probably not ideal, we should figure out what this error
-- means and act accordingly.
remoteCallFailureTMC :: TestTree
remoteCallFailureTMC =
  testCase "should respond with error when facing GRpcTooMuchConcurrency" $
    runM . evalMock @Remote @IO $ do
      mockDiscoverAndCallReturns @IO (const $ pure (Right (GRpcTooMuchConcurrency (TooMuchConcurrency 2))))
      let remoteCall = RemoteCall validDomainText (Just validLocalPart)

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
      let remoteCall = RemoteCall validDomainText (Just validLocalPart)

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
      let remoteCall = RemoteCall validDomainText (Just validLocalPart)

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
      let remoteCall = RemoteCall validDomainText (Just validLocalPart)

      res <- mock @Remote @IO $ callRemote remoteCall

      actualCalls <- mockDiscoverAndCallCalls @IO
      let expectedCall = ValidatedRemoteCall (Domain validDomainText) validLocalPart
      embed $ assertEqual "one remote call should be made" [expectedCall] actualCalls
      embed $ assertBool "the response should have error" (isResponseError res)

isResponseError :: Response -> Bool
isResponseError (ResponseErr _) = True
isResponseError (ResponseHTTPResponse _) = False

isRight' :: Validation a b -> Bool
isRight' = isRight . validationToEither

validLocalPart :: LocalCall
validLocalPart = LocalCall Brig (HTTPMethod HTTP.GET) "/users" [QueryParam "handle" "foo"] mempty

validDomainText :: Text
validDomainText = "example.com"

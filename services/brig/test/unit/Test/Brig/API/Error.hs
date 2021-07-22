module Test.Brig.API.Error where

import Brig.API.Error
import Data.Domain
import Imports
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Utilities as Wai
import qualified Servant.Client.Core as Servant
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Wire.API.Federation.Client
import Wire.API.Federation.Error
import qualified Wire.API.Federation.GRPC.Types as Proto

tests :: TestTree
tests = testGroup "Brig.API.Error" [testFedError, testOutwardError]

testFedError :: TestTree
testFedError =
  testGroup
    "fedEror"
    [ testCase "when federation is unavailable" $
        assertFedErrorStatus (FederationUnavailable "federator down!") 500,
      testCase "when federation is not implemented" $
        assertFedErrorStatus FederationNotImplemented 403,
      testCase "when federation is not configured" $
        assertFedErrorStatus FederationNotConfigured 400,
      testCase "when federation call fails due to RPC error" $
        assertFedErrorStatus (mkFailure (FederationClientRPCError "some failure")) 500,
      testCase "when federation call fails due wrong method" $
        assertFedErrorStatus (mkFailure (FederationClientInvalidMethod "GET")) 500,
      testCase "when federation call fails due to requesting streaming" $
        assertFedErrorStatus (mkFailure FederationClientStreamingUnsupported) 500,
      testCase "when federation call fails due to discovery failure" $ do
        let outwardErr = FederationClientOutwardError (Proto.OutwardError Proto.DiscoveryFailed Nothing)
        assertFedErrorStatus (mkFailure outwardErr) 500,
      testCase "when federation call fails due to decode failure" $
        assertFedErrorStatus (mkFailure (FederationClientServantError (Servant.DecodeFailure "some failure" emptyRes))) 533,
      testCase "when federation call fails due to Servant.FailureResponse" $
        assertFedErrorStatus (mkFailure (FederationClientServantError (Servant.FailureResponse emptyReq emptyRes))) 533,
      testCase "when federation call fails due to invalid content type" $
        assertFedErrorStatus (mkFailure (FederationClientServantError (Servant.InvalidContentTypeHeader emptyRes))) 533,
      testCase "when federation call fails due to unsupported content type" $
        assertFedErrorStatus (mkFailure (FederationClientServantError (Servant.UnsupportedContentType "application/xml" emptyRes))) 533,
      testCase "when federation call fails due to connection error" $
        assertFedErrorStatus (mkFailure (FederationClientServantError (Servant.ConnectionError (SomeException TestException)))) 500
    ]

testOutwardError :: TestTree
testOutwardError =
  testGroup "federationRemoteError" $
    [ testGroup "status" $
        [ testCase "when remote is not found" $
            assertOutwardErrorStatus Proto.RemoteNotFound 422,
          testCase "when discovery fails" $
            assertOutwardErrorStatus Proto.DiscoveryFailed 500,
          testCase "when connection is refused" $
            assertOutwardErrorStatus Proto.ConnectionRefused 521,
          testCase "when TLS fails" $
            assertOutwardErrorStatus Proto.TLSFailure 525,
          testCase "when remote returns version mismatch" $
            assertOutwardErrorStatus Proto.VersionMismatch 531,
          testCase "when remote denies federation" $
            assertOutwardErrorStatus Proto.FederationDeniedByRemote 532,
          testCase "when local federator denies federation" $
            assertOutwardErrorStatus Proto.FederationDeniedLocally 400,
          testCase "when remote federator errors" $
            assertOutwardErrorStatus Proto.RemoteFederatorError 533,
          testCase "when federator returns invalid request" $
            assertOutwardErrorStatus Proto.InvalidRequest 500
        ],
      testGroup "label & message" $
        [ testCase "when error payload is specified" $ do
            let outwardErr = Proto.OutwardError Proto.TLSFailure . Just $ Proto.ErrorPayload "an-interesting-label" "very interesting message"
                waiErr = federationRemoteError outwardErr
            assertEqual "label should be copied" (Wai.label waiErr) "an-interesting-label"
            assertEqual "message should be copied" (Wai.message waiErr) "very interesting message",
          testCase "when error payload is not specified" $ do
            let outwardErr = Proto.OutwardError Proto.TLSFailure Nothing
                waiErr = federationRemoteError outwardErr
            assertEqual "label should be set as unknown" (Wai.label waiErr) "unknown-federation-error"
            assertEqual "message should be set as unknown" (Wai.message waiErr) "Unknown federation error"
        ]
    ]

mkFailure :: FederationClientError -> FederationError
mkFailure =
  FederationCallFailure
    . FederationClientFailure (Domain "far-away.example.com") "/federation/test"

assertFedErrorStatus :: HasCallStack => FederationError -> Int -> IO ()
assertFedErrorStatus err sts = assertEqual ("http status should be " <> show sts) (statusFor err) sts

assertOutwardErrorStatus :: HasCallStack => Proto.OutwardErrorType -> Int -> IO ()
assertOutwardErrorStatus errType sts =
  assertEqual ("http status should be " <> show sts) (HTTP.statusCode . Wai.code . federationRemoteError $ Proto.OutwardError errType Nothing) sts

statusFor :: FederationError -> Int
statusFor = HTTP.statusCode . errorStatus . fedError

emptyReq :: Servant.RequestF () (Servant.BaseUrl, ByteString)
emptyReq =
  Servant.Request
    { Servant.requestPath = (Servant.BaseUrl Servant.Http "" 0 "", ""),
      Servant.requestQueryString = mempty,
      Servant.requestBody = Nothing,
      Servant.requestAccept = mempty,
      Servant.requestHeaders = mempty,
      Servant.requestHttpVersion = HTTP.http11,
      Servant.requestMethod = HTTP.methodGet
    }

emptyRes :: Servant.Response
emptyRes = Servant.Response HTTP.status200 mempty HTTP.http11 ""

data TestException = TestException
  deriving (Show, Eq)

instance Exception TestException

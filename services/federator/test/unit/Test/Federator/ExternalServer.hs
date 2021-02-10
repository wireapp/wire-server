{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Federator.ExternalServer where

import Federator.Brig (Brig)
import Federator.ExternalServer (callLocal)
import Imports
import qualified Network.HTTP.Types as HTTP
import Polysemy (embed, runM)
import Test.Polysemy.Mock (Mock (mock), evalMock)
import Test.Polysemy.Mock.TH (genMock)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Wire.API.Federation.GRPC.Types

genMock ''Brig

tests :: TestTree
tests =
  testGroup "InternalServer" $
    [ localCallBrigSuccess
    ]

-- TODO: Add more tests
localCallBrigSuccess :: TestTree
localCallBrigSuccess =
  testCase "should sucessfully return on HTTP 200" $
    runM . evalMock @Brig @IO $ do
      mockBrigCallReturns @IO (\_ _ _ _ -> pure (HTTP.status200, Just "response body"))
      let request = LocalCall Brig (HTTPMethod HTTP.GET) "/users" [QueryParam "handle" "foo"] mempty

      res <- mock @Brig @IO $ callLocal request

      actualCalls <- mockBrigCallCalls @IO
      let expectedCall = (HTTP.GET, "/users", [QueryParam "handle" "foo"], mempty)
      embed $ assertEqual "one call to brig should be made" [expectedCall] actualCalls
      embed $ assertEqual "response should be success with correct body" (ResponseHTTPResponse (HTTPResponse 200 "response body")) res

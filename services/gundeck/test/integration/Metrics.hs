module Metrics where

import Imports
import TestSetup
import Bilge
import Bilge.Assert
import Test.Tasty
import Test.Tasty.HUnit
import Control.Lens ((^.))

type TestSignature a = Gundeck -> Http a

test :: IO TestSetup -> TestName -> (TestSignature a) -> TestTree
test setup n h = testCase n runTest
  where
    runTest = do
        s <- setup
        void $ runHttpT (s ^. tsManager) (h (s ^. tsGundeck))

tests :: IO TestSetup -> TestTree
tests s = testGroup "Metrics" [test s "prometheus" testPrometheusMetrics]

testPrometheusMetrics :: Gundeck -> Http ()
testPrometheusMetrics gundeck = do
    get (runGundeck gundeck . path "/i/metrics") !!! do
        const 200 === statusCode
        -- Should contain the request duration metric in its output
        const (Just "TYPE http_request_duration_seconds histogram") =~= responseBody


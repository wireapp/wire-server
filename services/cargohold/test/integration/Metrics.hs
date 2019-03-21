module Metrics (tests) where

import Imports
import Bilge
import Bilge.Assert
import TestSetup
import Test.Tasty

tests :: IO TestSetup -> TestTree
tests s = testGroup "Metrics" [test s "prometheus" testPrometheusMetrics]

testPrometheusMetrics :: TestSignature ()
testPrometheusMetrics cargohold =
    get (cargohold . path "/i/metrics") !!! do
        const 200 === statusCode
        -- Should contain the request duration metric in its output
        const (Just "TYPE http_request_duration_seconds histogram") =~= responseBody



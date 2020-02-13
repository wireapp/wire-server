module Metrics where

import Bilge
import Bilge.Assert
import Control.Lens (view)
import Imports
import Test.Tasty
import TestSetup

type TestSignature a = GundeckR -> Http a

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "Metrics"
    [ test s "prometheus gundeck" testPrometheusMetricsGundeck,
      test s "prometheus cannon" testPrometheusMetricsCannon
    ]

testPrometheusMetricsGundeck :: TestM ()
testPrometheusMetricsGundeck = do
  gundeck <- view tsGundeck
  get (runGundeckR gundeck . path "/i/metrics") !!! do
    const 200 === statusCode
    -- Should contain the request duration metric in its output
    const (Just "TYPE http_request_duration_seconds histogram") =~= responseBody

testPrometheusMetricsCannon :: TestM ()
testPrometheusMetricsCannon = do
  cannon <- view tsCannon
  get (runCannonR cannon . path "/i/metrics") !!! do
    const 200 === statusCode
    -- Should contain the request duration metric in its output
    const (Just "TYPE http_request_duration_seconds histogram") =~= responseBody

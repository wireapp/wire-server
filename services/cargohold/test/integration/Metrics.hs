module Metrics (tests) where

tests :: IO TestSetup -> TestTree
tests s = test s "prometheus" testPrometheusMetrics

testPrometheusMetrics :: TestSignature
testPrometheusMetrics cargohold =
    g <- view cargohold
    get (g . path "/i/metrics") !!! do
        const 200 === statusCode
        -- Should contain the request duration metric in its output
        const (Just "TYPE http_request_duration_seconds histogram") =~= responseBody



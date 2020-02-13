{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- | See also: services/spar/test-integration/Test/Spar/APISpec.hs
module API.Metrics
  ( tests,
  )
where

import Bilge
import Bilge.Assert
import Brig.Types.User
import Data.Attoparsec.Text
import Data.ByteString.Conversion
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Util

tests :: Manager -> Brig -> IO TestTree
tests manager brig = do
  return $
    testGroup
      "metrics"
      [ testCase "prometheus" . void $ runHttpT manager (testPrometheusMetrics brig),
        testCase "work" . void $ runHttpT manager (testMetricsEndpoint brig)
      ]

testPrometheusMetrics :: Brig -> Http ()
testPrometheusMetrics brig = do
  get (brig . path "/i/metrics") !!! do
    const 200 === statusCode
    -- Should contain the request duration metric in its output
    const (Just "TYPE http_request_duration_seconds histogram") =~= responseBody

testMetricsEndpoint :: Brig -> Http ()
testMetricsEndpoint brig = do
  let p1 = "/self"
      p2 uid = "/users/" <> uid <> "/clients"
  beforeSelf <- getCount "/self"
  beforeClients <- getCount "/users/:uid/clients"
  uid <- userId <$> randomUser brig
  uid' <- userId <$> randomUser brig
  _ <- get (brig . path p1 . zAuthAccess uid "conn" . expect2xx)
  _ <- get (brig . path (p2 $ toByteString' uid) . zAuthAccess uid "conn" . expect2xx)
  _ <- get (brig . path (p2 $ toByteString' uid') . zAuthAccess uid "conn" . expect2xx)
  countSelf <- getCount "/self"
  liftIO $ assertEqual "/self was called once" (beforeSelf + 1) countSelf
  countClients <- getCount "/users/:uid/clients"
  liftIO $ assertEqual "/users/:uid/clients was called twice" (beforeClients + 2) countClients
  where
    getCount endpoint = do
      rsp <- responseBody <$> get (brig . path "i/metrics")
      -- is there some responseBodyAsText function used elsewhere?
      let asText = fromMaybe "" (fromByteString' (fromMaybe "" rsp))
      return $ fromRight 0 (parseOnly (parseCount endpoint) asText)
    parseCount :: Text -> Parser Integer
    parseCount endpoint =
      manyTill anyChar (string ("http_request_duration_seconds_count{handler=\"" <> endpoint <> "\",method=\"GET\",status_code=\"200\"} "))
        *> decimal
-- FUTUREWORK: check whether prometheus metrics are correct regarding timings:

-- Do we have a bug here?

-- Making two requests to POST /i/users leads to

-- http_request_duration_seconds_bucket{handler="/i/users",method="POST",status_code="201",le="0.005"} 0
-- http_request_duration_seconds_bucket{handler="/i/users",method="POST",status_code="201",le="0.01"} 0
-- http_request_duration_seconds_bucket{handler="/i/users",method="POST",status_code="201",le="0.025"} 0
-- http_request_duration_seconds_bucket{handler="/i/users",method="POST",status_code="201",le="0.05"} 0
-- http_request_duration_seconds_bucket{handler="/i/users",method="POST",status_code="201",le="0.1"} 0
-- http_request_duration_seconds_bucket{handler="/i/users",method="POST",status_code="201",le="0.25"} 1
-- http_request_duration_seconds_bucket{handler="/i/users",method="POST",status_code="201",le="0.5"} 2
-- http_request_duration_seconds_bucket{handler="/i/users",method="POST",status_code="201",le="1.0"} 2
-- http_request_duration_seconds_bucket{handler="/i/users",method="POST",status_code="201",le="2.5"} 2
-- http_request_duration_seconds_bucket{handler="/i/users",method="POST",status_code="201",le="5.0"} 2
-- http_request_duration_seconds_bucket{handler="/i/users",method="POST",status_code="201",le="10.0"} 2
-- http_request_duration_seconds_bucket{handler="/i/users",method="POST",status_code="201",le="+Inf"} 2
-- http_request_duration_seconds_sum{handler="/i/users",method="POST",status_code="201"} 0.60430278
-- http_request_duration_seconds_count{handler="/i/users",method="POST",status_code="201"} 2

-- i.e. all the "buckets" above "le 0.5 seconds" contain 2 elements, which seems incorrect? (or am I misunderstanding prometheus here?)

{- FUTUREWORK:

- test that different verbs on the same path end in different buckets, eg.:

>>> put (brig . path p1 . zAuthAccess uid "conn" . expect2xx . json _)

- check that the correct path gets "tracked", i.e.
  if u make one GET request to /users, which should return a 404, check that
     "users": {
      "GET": {
        "status": {
          "404": 1
        },
        "time": {
          "480": 0,
          "85": 0,
          "0": 0,
          "679": 0,
          "30": 1,
          "170": 0,
          "339": 0,
          "1358": 0,
          "960": 0,
          "120": 0,
          "240": 0,
          "42": 0,
          "60": 0
        }
      }
    },
  there should be the count of 1 in users.GET.status.404 and 1 in one of the buckets below

-}

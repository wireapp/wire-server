{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

-- | See also: services/spar/test-integration/Test/Spar/APISpec.hs
module API.Metrics
  ( tests,
  )
where

import Bilge
import Bilge.Assert
import Data.Attoparsec.Text
import Data.ByteString.Conversion
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Util
import Wire.API.User
import Wire.ServerOptions.Brig qualified as Opt

tests :: Manager -> Opt.Opts -> Brig -> IO TestTree
tests manager opts brig = do
  pure $
    testGroup
      "metrics"
      [ testCase "prometheus" . void $ runHttpT manager (testPrometheusMetrics brig),
        testCase "work" . void $ runHttpT manager (testMetricsEndpoint opts brig)
      ]

testPrometheusMetrics :: Brig -> Http ()
testPrometheusMetrics brig = do
  get (brig . path "/i/metrics") !!! do
    const 200 === statusCode
    -- Should contain the request duration metric in its output
    const (Just "TYPE http_request_duration_seconds histogram") =~= responseBody

testMetricsEndpoint :: Opt.Opts -> Brig -> Http ()
testMetricsEndpoint opts brig0 = withSettingsOverrides opts $ do
  let brig = apiVersion "v1" . brig0
      p1 = "/self"
      p2 uid = "/users/" <> uid <> "/clients"
      p3 = "/login"
  beforeSelf <- getCount "/self" "GET"
  beforeClients <- getCount "/users/:uid/clients" "GET"
  beforeProperties <- getCount "/login" "POST"
  (uid, Just email) <- (\u -> (userId u, userEmail u)) <$> randomUser brig0
  uid' <- userId <$> randomUser brig0
  _ <- get (brig . path p1 . zAuthAccess uid "conn" . expect2xx)
  _ <- get (brig . path (p2 $ toByteString' uid) . zAuthAccess uid "conn" . expect2xx)
  _ <- get (brig . path (p2 $ toByteString' uid') . zAuthAccess uid "conn" . expect2xx)
  _ <- post (brig . path p3 . contentJson . queryItem "persist" "true" . json (defEmailLogin email) . expect2xx)
  _ <- post (brig . path p3 . contentJson . queryItem "persist" "true" . json (defEmailLogin email) . expect2xx)
  countSelf <- getCount "/self" "GET"
  liftIO $ assertBool "/self was called at least once" ((beforeSelf + 1) <= countSelf)
  countClients <- getCount "/users/:uid/clients" "GET"
  liftIO $ assertBool "/users/:uid/clients was called at least twice" ((beforeClients + 2) <= countClients)
  countProperties <- getCount "/login" "POST"
  liftIO $ assertBool "/login was called at least twice" ((beforeProperties + 2) <= countProperties)
  where
    getCount endpoint m = do
      rsp <- responseBody <$> get (brig0 . path "i/metrics")
      -- is there some responseBodyAsText function used elsewhere?
      let asText = fromMaybe "" (fromByteString' (fromMaybe "" rsp))
      pure $ fromRight 0 (parseOnly (parseCount endpoint m) asText)
    parseCount :: Text -> Text -> Parser Integer
    parseCount endpoint m =
      manyTill anyChar (string ("http_request_duration_seconds_count{handler=\"" <> endpoint <> "\",method=\"" <> m <> "\",status_code=\"200\"} "))
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

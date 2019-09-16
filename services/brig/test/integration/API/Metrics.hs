{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

-- | See also: services/spar/test-integration/Test/Spar/APISpec.hs
module API.Metrics (tests) where

import Imports
import Bilge
import Bilge.Assert
import Brig.Types.User
import Control.Lens
import Data.ByteString.Conversion
import Data.String.Conversions (cs)
import Data.Aeson
import Data.Aeson.Lens
import Data.Set as Set
import Test.Tasty
import Test.Tasty.HUnit
import Util


tests :: Manager -> Brig -> IO TestTree
tests manager brig = do
    return $ testGroup "metrics"
        [ testCase "prometheus" . void $ runHttpT manager (testPrometheusMetrics brig)
        , testCase "work" . void $ runHttpT manager (testMonitoringEndpoint brig)
        ]

testPrometheusMetrics :: Brig -> Http ()
testPrometheusMetrics brig = do
    get (brig . path "/i/metrics") !!! do
        const 200 === statusCode
        -- Should contain the request duration metric in its output
        const (Just "TYPE http_request_duration_seconds histogram") =~= responseBody

testMonitoringEndpoint :: Brig -> Http ()
testMonitoringEndpoint brig = do
    let p1 = "/self"
        p2 uid = "/users/" <> uid <> "/clients"

    uid <- userId <$> randomUser brig
    uid' <- userId <$> randomUser brig
    _ <- get (brig . path p1 . zAuthAccess uid "conn" . expect2xx)
    _ <- get (brig . path (p2 $ toByteString' uid) . zAuthAccess uid "conn" . expect2xx)
    _ <- get (brig . path (p2 $ toByteString' uid') . zAuthAccess uid "conn" . expect2xx)

    resp :: Value <- responseJsonUnsafe <$> get (brig . path "i/monitoring")
    let have :: Set Text = Set.fromList $ fst <$> (resp ^@.. key "net" . key "resources" . members)
        want :: Set Text = Set.fromList $ cs <$> [p1, p2 ":uid"]
        errmsg = "some of " <> show want <> " missing in metrics: " <> show have
    liftIO $ assertBool errmsg (want `Set.isSubsetOf` have)


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

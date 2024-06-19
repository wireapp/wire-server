{-# LANGUAGE NoMonomorphismRestriction #-}

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

-- | See also: services/brig/test/integration/API/Metrics.hs
module Test.MetricsSpec
  ( spec,
  )
where

import Bilge
import Control.Lens
import Data.String.Conversions
import Imports
import Util

spec :: (HasCallStack) => SpecWith TestEnv
spec = describe "metrics" . it "works" $ do
  spar <- asks (^. teSpar)
  let p1 = "/sso/metadata"
      p2 idpid = "/sso/initiate-login/" <> idpid
  _ <- call $ get (spar . path p1)
  _ <- call $ get (spar . path (p2 "316f1c18-2980-11e9-ab0b-ef604d1791b2"))
  _ <- call $ get (spar . path (p2 "60a7dda8-2980-11e9-b359-fb5b41565453"))
  resp :: String <- call $ foldMap cs . responseBody <$> get (spar . path "i/metrics")
  -- FUTUREWORK: here we could parse the prometheus 'RegistrySample' and inspect it more
  -- thoroughly, but i'm not sure there is a parser.
  liftIO $ do
    (resp `shouldContain`)
      `mapM_` [ p1,
                p2 ":idp",
                "http_request_duration_seconds_bucket",
                "handler=",
                "method=",
                "status_code=",
                "le="
              ]

{- sample value:
 # HELP http_request_duration_seconds The HTTP request latencies in seconds.
 # TYPE http_request_duration_seconds histogram
http_request_duration_seconds_bucket{handler="i/status",method="GET",status_code="200",le="0.005"} 2
http_request_duration_seconds_bucket{handler="i/status",method="GET",status_code="200",le="0.01"} 2
http_request_duration_seconds_bucket{handler="i/status",method="GET",status_code="200",le="0.025"} 2
http_request_duration_seconds_bucket{handler="i/status",method="GET",status_code="200",le="0.05"} 2
http_request_duration_seconds_bucket{handler="i/status",method="GET",status_code="200",le="0.1"} 2
http_request_duration_seconds_bucket{handler="i/status",method="GET",status_code="200",le="0.25"} 2
http_request_duration_seconds_bucket{handler="i/status",method="GET",status_code="200",le="0.5"} 2
http_request_duration_seconds_bucket{handler="i/status",method="GET",status_code="200",le="1.0"} 2
http_request_duration_seconds_bucket{handler="i/status",method="GET",status_code="200",le="2.5"} 2
http_request_duration_seconds_bucket{handler="i/status",method="GET",status_code="200",le="5.0"} 2
http_request_duration_seconds_bucket{handler="i/status",method="GET",status_code="200",le="10.0"} 2
http_request_duration_seconds_bucket{handler="i/status",method="GET",status_code="200",le="+Inf"} 2
http_request_duration_seconds_sum{handler="i/status",method="GET",status_code="200"} 1.75926e-4
http_request_duration_seconds_count{handler="i/status",method="GET",status_code="200"} 2
http_request_duration_seconds_bucket{handler="sso/initiate-login/<>",method="GET",status_code="404",le="0.005"} 2
http_request_duration_seconds_bucket{handler="sso/initiate-login/<>",method="GET",status_code="404",le="0.01"} 2
http_request_duration_seconds_bucket{handler="sso/initiate-login/<>",method="GET",status_code="404",le="0.025"} 2
http_request_duration_seconds_bucket{handler="sso/initiate-login/<>",method="GET",status_code="404",le="0.05"} 2
http_request_duration_seconds_bucket{handler="sso/initiate-login/<>",method="GET",status_code="404",le="0.1"} 2
http_request_duration_seconds_bucket{handler="sso/initiate-login/<>",method="GET",status_code="404",le="0.25"} 2
http_request_duration_seconds_bucket{handler="sso/initiate-login/<>",method="GET",status_code="404",le="0.5"} 2
http_request_duration_seconds_bucket{handler="sso/initiate-login/<>",method="GET",status_code="404",le="1.0"} 2
http_request_duration_seconds_bucket{handler="sso/initiate-login/<>",method="GET",status_code="404",le="2.5"} 2
http_request_duration_seconds_bucket{handler="sso/initiate-login/<>",method="GET",status_code="404",le="5.0"} 2
http_request_duration_seconds_bucket{handler="sso/initiate-login/<>",method="GET",status_code="404",le="10.0"} 2
http_request_duration_seconds_bucket{handler="sso/initiate-login/<>",method="GET",status_code="404",le="+Inf"} 2
http_request_duration_seconds_sum{handler="sso/initiate-login/<>",method="GET",status_code="404"} 5.37109e-3
http_request_duration_seconds_count{handler="sso/initiate-login/<>",method="GET",status_code="404"} 2
http_request_duration_seconds_bucket{handler="sso/metadata",method="GET",status_code="200",le="0.005"} 1
http_request_duration_seconds_bucket{handler="sso/metadata",method="GET",status_code="200",le="0.01"} 1
http_request_duration_seconds_bucket{handler="sso/metadata",method="GET",status_code="200",le="0.025"} 1
http_request_duration_seconds_bucket{handler="sso/metadata",method="GET",status_code="200",le="0.05"} 1
http_request_duration_seconds_bucket{handler="sso/metadata",method="GET",status_code="200",le="0.1"} 1
http_request_duration_seconds_bucket{handler="sso/metadata",method="GET",status_code="200",le="0.25"} 1
http_request_duration_seconds_bucket{handler="sso/metadata",method="GET",status_code="200",le="0.5"} 1
http_request_duration_seconds_bucket{handler="sso/metadata",method="GET",status_code="200",le="1.0"} 1
http_request_duration_seconds_bucket{handler="sso/metadata",method="GET",status_code="200",le="2.5"} 1
http_request_duration_seconds_bucket{handler="sso/metadata",method="GET",status_code="200",le="5.0"} 1
http_request_duration_seconds_bucket{handler="sso/metadata",method="GET",status_code="200",le="10.0"} 1
http_request_duration_seconds_bucket{handler="sso/metadata",method="GET",status_code="200",le="+Inf"} 1
http_request_duration_seconds_sum{handler="sso/metadata",method="GET",status_code="200"} 1.51973e-4
http_request_duration_seconds_count{handler="sso/metadata",method="GET",status_code="200"} 1
-}

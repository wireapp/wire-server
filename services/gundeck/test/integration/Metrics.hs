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

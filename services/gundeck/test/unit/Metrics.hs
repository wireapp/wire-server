-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Metrics (tests) where

import Control.Lens ((.~))
import Data.Id
import Data.List.NonEmpty qualified as NonEmpty
import Data.String.Conversions (cs)
import Gundeck.Notification.Metrics
import Imports
import Prometheus qualified as Prom
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Internal.Notification

tests :: TestTree
tests =
  testGroup
    "notification metrics"
    [testCase "observeStored and observeFetch update the exported metrics" observeTest]

allClientsTarget :: IO NotificationTarget
allClientsTarget = target <$> randomId

someClientsTarget :: Int -> IO NotificationTarget
someClientsTarget n = do
  t <- allClientsTarget
  pure (t & targetClients .~ map (ClientId . fromIntegral) [1 .. n])

-- | Value of a sample (e.g. @push_stored_targets_sum@) in the exported
-- metrics. A metric is only registered when first used, so a missing sample
-- counts as 0.
sample :: String -> IO Double
sample name = do
  exported <- cs <$> Prom.exportMetricsAsText
  case [read v | l <- lines exported, [k, v] <- [words l], k == name] of
    [] -> pure 0
    [v] -> pure v
    vs -> assertFailure ("expected exactly one sample for " <> name <> ", got " <> show (length vs))

-- TODO: Split this test function up.
-- TODO: Consider to intialize the counters
-- TODO: WebSocket metrics untested
-- TODO: Reflect the modules structure of prod code

-- | All assertions are in one test case, because the metrics are global and
-- the test cases could otherwise run in parallel and disturb each other's
-- deltas.
observeTest :: Assertion
observeTest = do
  let expectedDeltas =
        [ ("push_stored_targets_count", 1),
          ("push_stored_targets_sum", 3),
          ("push_stored_targets_all_clients", 1),
          ("push_stored_targets_some_clients", 2),
          ("push_stored_target_clients_sum", 4),
          ("push_stored_payload_bytes_sum", 100),
          ("notification_fetch", 2),
          ("notification_fetch_without_client", 1),
          ("notification_fetch_without_since", 1),
          ("notification_fetch_gap", 1),
          ("notification_fetch_has_more", 1),
          ("notification_fetch_returned_sum", 7)
        ]
      samples = traverse (sample . fst) expectedDeltas
  beforeVals <- samples
  tgts <- sequence [allClientsTarget, someClientsTarget 3, someClientsTarget 1]
  observeStored (NonEmpty.fromList tgts) 100
  observeFetch FetchStats {hasClient = False, hasSince = True, gap = True, hasMore = False, returned = 7}
  observeFetch FetchStats {hasClient = True, hasSince = False, gap = False, hasMore = True, returned = 0}
  afterVals <- samples
  zip (fst <$> expectedDeltas) (zipWith (-) afterVals beforeVals) @?= expectedDeltas

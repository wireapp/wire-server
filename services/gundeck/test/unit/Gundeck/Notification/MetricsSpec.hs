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

module Gundeck.Notification.MetricsSpec (tests) where

import Control.Lens ((.~))
import Data.Id
import Data.List.NonEmpty qualified as NonEmpty
import Gundeck.Notification.Metrics
import Imports
import MetricsHelper
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Internal.Notification

tests :: TestTree
tests =
  -- The metrics are global, so the test cases must not run in parallel.
  -- Otherwise they would disturb each other's deltas.
  dependentTestGroup
    "notification metrics"
    AllFinish
    [ testCase "observeStored counts targets" storedTargetsTest,
      testCase "observeStored distinguishes all and some clients" storedClientsTest,
      testCase "observeStored observes payload size" storedPayloadTest,
      testCase "observeFetch counts fetches without client and since" fetchWithoutTest,
      testCase "observeFetch counts gaps" fetchGapTest,
      testCase "observeFetch counts fetches with more" fetchHasMoreTest,
      testCase "observeFetch observes returned notifications" fetchReturnedTest
    ]

allClientsTarget :: IO NotificationTarget
allClientsTarget = target <$> randomId

someClientsTarget :: Int -> IO NotificationTarget
someClientsTarget n = do
  t <- allClientsTarget
  pure (t & targetClients .~ map (ClientId . fromIntegral) [1 .. n])

-- TODO: Consider to intialize the counters

-- | Store a push with one target for all clients, and two for some clients (3
-- and 1) with a payload of the given size.
storeMixed :: Int64 -> IO ()
storeMixed payloadBytes = do
  tgts <- sequence [allClientsTarget, someClientsTarget 3, someClientsTarget 1]
  observeStored (NonEmpty.fromList tgts) payloadBytes

-- | A fetch that does not trigger any of the counters.
plainFetch :: FetchStats
plainFetch = FetchStats {hasClient = True, hasSince = True, gap = False, hasMore = False, returned = 0}

storedTargetsTest :: Assertion
storedTargetsTest = do
  ds <- deltas ["push_stored_targets_count", "push_stored_targets_sum"] (storeMixed 0)
  ds @?= [("push_stored_targets_count", 1), ("push_stored_targets_sum", 3)]

storedClientsTest :: Assertion
storedClientsTest = do
  ds <-
    deltas
      ["push_stored_targets_all_clients", "push_stored_targets_some_clients", "push_stored_target_clients_sum"]
      (storeMixed 0)
  ds
    @?= [ ("push_stored_targets_all_clients", 1),
          ("push_stored_targets_some_clients", 2),
          ("push_stored_target_clients_sum", 4)
        ]

storedPayloadTest :: Assertion
storedPayloadTest = do
  ds <- deltas ["push_stored_payload_bytes_sum"] (storeMixed 100)
  ds @?= [("push_stored_payload_bytes_sum", 100)]

fetchWithoutTest :: Assertion
fetchWithoutTest = do
  ds <-
    deltas
      ["notification_fetch", "notification_fetch_without_client", "notification_fetch_without_since"]
      (observeFetch plainFetch {hasClient = False, hasSince = False})
  ds
    @?= [ ("notification_fetch", 1),
          ("notification_fetch_without_client", 1),
          ("notification_fetch_without_since", 1)
        ]

fetchGapTest :: Assertion
fetchGapTest = do
  ds <- deltas ["notification_fetch_gap"] (observeFetch plainFetch {gap = True})
  ds @?= [("notification_fetch_gap", 1)]

fetchHasMoreTest :: Assertion
fetchHasMoreTest = do
  ds <- deltas ["notification_fetch_has_more"] (observeFetch plainFetch {hasMore = True})
  ds @?= [("notification_fetch_has_more", 1)]

fetchReturnedTest :: Assertion
fetchReturnedTest = do
  ds <- deltas ["notification_fetch_returned_sum"] (observeFetch plainFetch {returned = 7})
  ds @?= [("notification_fetch_returned_sum", 7)]

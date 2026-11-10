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
  testGroup
    "notification metrics"
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
storedTargetsTest =
  assertIncreasedAtLeast
    [("wire_push_stored_targets_count", 1), ("wire_push_stored_targets_sum", 3)]
    (storeMixed 0)

storedClientsTest :: Assertion
storedClientsTest =
  assertIncreasedAtLeast
    [ ("wire_push_stored_targets_all_clients", 1),
      ("wire_push_stored_targets_some_clients", 2),
      ("wire_push_stored_target_clients_sum", 4)
    ]
    (storeMixed 0)

storedPayloadTest :: Assertion
storedPayloadTest =
  assertIncreasedAtLeast
    [("wire_push_stored_payload_bytes_sum", 100)]
    (storeMixed 100)

fetchWithoutTest :: Assertion
fetchWithoutTest =
  assertIncreasedAtLeast
    [ ("wire_notification_fetch", 1),
      ("wire_notification_fetch_without_client", 1),
      ("wire_notification_fetch_without_since", 1)
    ]
    (observeFetch plainFetch {hasClient = False, hasSince = False})

fetchGapTest :: Assertion
fetchGapTest =
  assertIncreasedAtLeast
    [("wire_notification_fetch_gap", 1)]
    (observeFetch plainFetch {gap = True})

fetchHasMoreTest :: Assertion
fetchHasMoreTest =
  assertIncreasedAtLeast
    [("wire_notification_fetch_has_more", 1)]
    (observeFetch plainFetch {hasMore = True})

fetchReturnedTest :: Assertion
fetchReturnedTest =
  assertIncreasedAtLeast
    [("wire_notification_fetch_returned_sum", 7)]
    (observeFetch plainFetch {returned = 7})

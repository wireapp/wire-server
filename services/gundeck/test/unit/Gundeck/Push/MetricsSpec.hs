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

module Gundeck.Push.MetricsSpec (tests) where

import Data.Id
import Data.List.NonEmpty (NonEmpty ((:|)))
import Gundeck.Push.Metrics
import Imports
import MetricsHelper
import MockGundeck (fakePresence)
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Internal.Notification
import Wire.API.Presence

tests :: TestTree
tests =
  -- The metrics are global, so the test cases must not run in parallel.
  -- Otherwise they would disturb each other's deltas.
  dependentTestGroup
    "push metrics"
    AllFinish
    [ testCase "monitorBadCannons counts unreachable presences" badCannonsTest,
      testCase "monitorDelivered counts delivered presences" deliveredTest,
      testCase "monitorDelivered counts transient deliveries" deliveredTransientTest
    ]

presence :: IO Presence
presence = (`fakePresence` ClientId 1) <$> randomId

notification :: Bool -> IO Notification
notification transient = do
  nid <- randomId
  pure (Notification nid transient (mempty :| []))

badCannonsTest :: Assertion
badCannonsTest = do
  prcs <- sequence [presence, presence]
  ds <- deltas ["push_ws_unreachable"] (monitorBadCannons ((), ((), prcs)))
  ds @?= [("push_ws_unreachable", 2)]

-- | One transient notification with one presence, and one non-transient with
-- two. All of them are delivered.
monitorMixed :: IO ()
monitorMixed = do
  transient <- notification True
  persistent <- notification False
  p1 <- presence
  p2 <- presence
  p3 <- presence
  monitorDelivered
    [(transient, [p1]), (persistent, [p2, p3])]
    [(ntfId transient, p1), (ntfId persistent, p2), (ntfId persistent, p3)]

deliveredTest :: Assertion
deliveredTest = do
  ds <- deltas ["push_ws_delivered_presences"] monitorMixed
  ds @?= [("push_ws_delivered_presences", 3)]

deliveredTransientTest :: Assertion
deliveredTransientTest = do
  ds <- deltas ["push_ws_delivered_presences_transient"] monitorMixed
  ds @?= [("push_ws_delivered_presences_transient", 1)]

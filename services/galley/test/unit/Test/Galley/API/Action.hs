-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foun(Domain "a.com")tion, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Test.Galley.API.Action where

import Data.Domain
import Data.Qualified
import Data.Set qualified as Set
import Galley.API.Action
import Imports
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))
import Wire.API.Federation.API.Brig
import Wire.API.FederationStatus

tests :: TestTree
tests =
  testGroup
    "firstConflictOrFullyConnected"
    ( ( \(name, responses, expected) ->
          testCase name $ firstConflictOrFullyConnected responses @?= expected
      )
        <$> testTable
    )

testTable :: [(String, [Remote NonConnectedBackends], FederationStatus)]
testTable =
  [ ("empty", [], FullyConnected),
    ("single response", [mkResponse (Domain "a.com") []], FullyConnected),
    ("multiple responses", [mkResponse (Domain "a.com") [], mkResponse (Domain "b.com") []], FullyConnected),
    ("single bad responses", [mkResponse (Domain "a.com") [Domain "b.com"]], NotConnectedDomains (Domain "a.com") (Domain "b.com")),
    ("one good one bad response", [mkResponse (Domain "a.com") [], mkResponse (Domain "b.com") [Domain "c.com"]], NotConnectedDomains (Domain "b.com") (Domain "c.com")),
    ("one bad one good response", [mkResponse (Domain "b.com") [Domain "c.com"], mkResponse (Domain "a.com") []], NotConnectedDomains (Domain "b.com") (Domain "c.com")),
    ("one bad multiple good responses", [mkResponse (Domain "b.com") [Domain "c.com"], mkResponse (Domain "a.com") [], mkResponse (Domain "d.com") []], NotConnectedDomains (Domain "b.com") (Domain "c.com")),
    ("multiple bad responses", [mkResponse (Domain "a.com") [Domain "b.com"], mkResponse (Domain "b.com") [Domain "a.com"]], NotConnectedDomains (Domain "a.com") (Domain "b.com"))
  ]
  where
    mkResponse :: Domain -> [Domain] -> Remote NonConnectedBackends
    mkResponse d = toRemoteUnsafe d . NonConnectedBackends . Set.fromList

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

module Test.Wire.API.Message (tests) where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Message (parseMap, parseMapMerge)

tests :: TestTree
tests =
  testGroup
    "Message"
    [ testParseMapDropsRecipientsOnDuplicateDomain,
      testParseMapMergeKeepsRecipientsOnDuplicateDomain
    ]

-- | Simulates the shape of a real OTR request: a list of qualified entries,
-- where each entry maps a "domain" to a set of "users" it targets. Nothing in
-- the wire protocol forbids two entries for the same domain -- e.g. a client
-- assembling a mixed-protocol proteus fallback message may independently emit
-- an entry for "recipients not yet migrated to MLS" and another for
-- "recipients needing legacy delivery for some other reason", both of which
-- can legitimately target the same domain.
sameDomainEntries :: [(String, Map String (Set Int))]
sameDomainEntries =
  [ ("a.example.com", Map.fromList [("alice", Set.fromList [1])]),
    ("a.example.com", Map.fromList [("bob", Set.fromList [2])]),
    ("b.example.com", Map.fromList [("carl", Set.fromList [3])])
  ]

-- | This is the bug: 'parseMap' is built on 'Map.fromList', which is
-- last-write-wins on duplicate keys. The second "a.example.com" entry
-- (bob) silently replaces the first (alice) instead of being combined with
-- it -- alice's recipients vanish with no error, no missing-clients report,
-- nothing. This is exactly how messages went missing for real users in
-- mixed-protocol conversations (see bug-report.md).
testParseMapDropsRecipientsOnDuplicateDomain :: TestTree
testParseMapDropsRecipientsOnDuplicateDomain =
  testCase "parseMap silently drops earlier entries for a repeated domain" $ do
    let result :: Either String (Map String (Map String (Set Int)))
        result = parseMap (Right . fst) (Right . snd) sameDomainEntries
    result
      @?= Right
        ( Map.fromList
            [ ("a.example.com", Map.fromList [("bob", Set.fromList [2])]), -- alice is gone!
              ("b.example.com", Map.fromList [("carl", Set.fromList [3])])
            ]
        )

-- | 'parseMapMerge' is the fix: it uses 'Map.fromListWith Map.union', so
-- entries for a repeated domain are merged instead of one clobbering the
-- other. Both alice and bob end up reachable under "a.example.com".
testParseMapMergeKeepsRecipientsOnDuplicateDomain :: TestTree
testParseMapMergeKeepsRecipientsOnDuplicateDomain =
  testCase "parseMapMerge merges entries for a repeated domain" $ do
    let result :: Either String (Map String (Map String (Set Int)))
        result = parseMapMerge (Right . fst) (Right . snd) sameDomainEntries
    result
      @?= Right
        ( Map.fromList
            [ ("a.example.com", Map.fromList [("alice", Set.fromList [1]), ("bob", Set.fromList [2])]),
              ("b.example.com", Map.fromList [("carl", Set.fromList [3])])
            ]
        )

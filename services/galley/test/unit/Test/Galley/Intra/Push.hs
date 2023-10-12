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

module Test.Galley.Intra.Push where

import Data.List1 qualified as List1
import Data.Monoid
import Galley.Intra.Push.Internal
import Imports
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

normalisePush :: PushTo a -> [PushTo a]
normalisePush p =
  map
    (\r -> p {_pushRecipients = List1.singleton r})
    (toList (_pushRecipients p))

chunkSize :: [PushTo a] -> Int
chunkSize = getSum . foldMap (Sum . length . _pushRecipients)

tests :: TestTree
tests =
  testGroup
    "chunkPushes"
    [ testProperty "empty push" $ \(Positive limit) ->
        chunkPushes limit [] === ([] :: [[PushTo ()]]),
      testProperty "no empty chunk" $ \(Positive limit) (pushes :: [PushTo Int]) ->
        not (any null (chunkPushes limit pushes)),
      testProperty "concatenation" $ \(Positive limit) (pushes :: [PushTo Int]) ->
        (chunkPushes limit pushes >>= reverse >>= normalisePush)
          === (pushes >>= normalisePush),
      testProperty "small chunks" $ \(Positive limit) (pushes :: [PushTo Int]) ->
        all ((<= limit) . chunkSize) (chunkPushes limit pushes)
    ]

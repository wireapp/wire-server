{-# LANGUAGE NumericUnderscores #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

-- | Tests for one-to-one conversations
module Test.Galley.API.One2One where

import Data.Id
import Data.List.Extra
import Data.Qualified
import Galley.API.One2One (one2OneConvId)
import Imports
import Test.Tasty
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "one2OneConvId"
    [ testProperty "symmetry" one2OneConvIdSymmetry,
      testCase "non-collision" one2OneConvIdNonCollision
    ]

one2OneConvIdSymmetry :: Qualified UserId -> Qualified UserId -> Property
one2OneConvIdSymmetry quid1 quid2 = one2OneConvId quid1 quid2 === one2OneConvId quid2 quid1

-- | Make sure that we never get the same conversation ID for a pair of
-- (assumingly) distinct qualified user IDs
one2OneConvIdNonCollision :: Assertion
one2OneConvIdNonCollision = do
  let len = 10_000
  -- A generator of lists of length 'len' of qualified user ID pairs
  let gen = vectorOf len arbitrary
  quids <- head <$> sample' gen
  anySame (fmap (uncurry one2OneConvId) quids) @?= False

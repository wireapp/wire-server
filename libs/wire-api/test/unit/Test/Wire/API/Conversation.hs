{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Wire.API.Conversation where

import qualified Data.Set as Set
import Imports
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty, (===))
import Wire.API.Conversation

tests :: TestTree
tests =
  testGroup
    "Conversation"
    [ accessRoleFromLegacyToV2ToLegacy,
      accessRoleFromV2ToLegacyToV2
    ]

accessRoleFromLegacyToV2ToLegacy :: TestTree
accessRoleFromLegacyToV2ToLegacy = testProperty "Access role conversion from legacy to v2 to legacy" p
  where
    p accessRoleLegacy =
      accessRoleLegacy === (toAccessRoleLegacy . fromAccessRoleLegacy) accessRoleLegacy

accessRoleFromV2ToLegacyToV2 :: TestTree
accessRoleFromV2ToLegacyToV2 =
  testProperty "Access role conversion from v2 to legacy to v2 - original should be a subset of roundtrip converted" p
  where
    p originalV2 = originalIsSubSetOfConverted && noSmallerLegacyIsSubsetOfOriginal
      where
        convertedToLegacy = toAccessRoleLegacy originalV2
        convertedBackToV2 = fromAccessRoleLegacy convertedToLegacy
        originalIsSubSetOfConverted = originalV2 `Set.isSubsetOf` convertedBackToV2
        smallerLegacy = fromAccessRoleLegacy <$> init [minBound .. convertedToLegacy]
        noSmallerLegacyIsSubsetOfOriginal = not (any (Set.isSubsetOf originalV2) smallerLegacy)

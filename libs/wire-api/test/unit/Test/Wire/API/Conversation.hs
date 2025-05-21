{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

module Test.Wire.API.Conversation where

import Data.Set qualified as Set
import Imports
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty, (===))
import Wire.API.Conversation hiding (AddPermissionUpdate)
import Wire.API.Event.Conversation

tests :: TestTree
tests =
  testGroup
    "Conversation"
    [ accessRoleFromLegacyToV2ToLegacy,
      accessRoleFromV2ToLegacyToV2,
      testIsCellsConversationEvent
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
        noSmallerLegacyIsSubsetOfOriginal = not (any (originalV2 `Set.isSubsetOf`) smallerLegacy)

testIsCellsConversationEvent :: TestTree
testIsCellsConversationEvent =
  testProperty "conversation event should be evaluated if relevant for cells" $
    \e ->
      case e of
        AddPermissionUpdate -> isCellsConversationEvent e === False
        ConvAccessUpdate -> isCellsConversationEvent e === False
        ConvCodeDelete -> isCellsConversationEvent e === False
        ConvCodeUpdate -> isCellsConversationEvent e === False
        ConvConnect -> isCellsConversationEvent e === False
        ConvCreate -> isCellsConversationEvent e === True
        ConvDelete -> isCellsConversationEvent e === True
        ConvReset -> isCellsConversationEvent e === False
        ConvMessageTimerUpdate -> isCellsConversationEvent e === False
        ConvReceiptModeUpdate -> isCellsConversationEvent e === False
        ConvRename -> isCellsConversationEvent e === True
        MemberJoin -> isCellsConversationEvent e === True
        MemberLeave -> isCellsConversationEvent e === True
        MemberStateUpdate -> isCellsConversationEvent e === True
        MLSMessageAdd -> isCellsConversationEvent e === False
        MLSWelcome -> isCellsConversationEvent e === False
        OtrMessageAdd -> isCellsConversationEvent e === False
        ProtocolUpdate -> isCellsConversationEvent e === False
        Typing -> isCellsConversationEvent e === False

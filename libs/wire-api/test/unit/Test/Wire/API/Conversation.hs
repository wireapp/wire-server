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
import Test.Tasty.QuickCheck (testProperty, (.&&.), (===))
import Wire.API.Conversation hiding (AddPermissionUpdate)
import Wire.API.Event.Conversation

tests :: TestTree
tests =
  testGroup
    "Conversation"
    [ accessRoleFromLegacyToV2ToLegacy,
      accessRoleFromV2ToLegacyToV2,
      testIsCellsConversationEvent,
      testToLegacyConversationMetadata,
      testToLegacyOwnConversation,
      testToLegacyConversation,
      testToLegacyConversationsResponse,
      testIsMeetingConversation
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
        ConvCreateMeeting -> isCellsConversationEvent e === True
        ConvDelete -> isCellsConversationEvent e === True
        ConvReset -> isCellsConversationEvent e === False
        ConvMessageTimerUpdate -> isCellsConversationEvent e === False
        ConvHistoryUpdate -> isCellsConversationEvent e === False
        ConvAdminlessReminder -> isCellsConversationEvent e === False
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
        MeetingCreate -> isCellsConversationEvent e === False
        MeetingUpdate -> isCellsConversationEvent e === False
        MeetingDelete -> isCellsConversationEvent e === False

--------------------------------------------------------------------------------
-- Legacy conversion tests

setGct :: Maybe GroupConvType -> ConversationMetadata GroupConvType -> ConversationMetadata GroupConvType
setGct gct meta = meta {cnvmGroupConvType = gct}

testToLegacyConversationMetadata :: TestTree
testToLegacyConversationMetadata =
  testGroup
    "toLegacyConversationMetadata"
    [ testProperty "converts GroupConversation -> GroupConversationLegacy" $
        \meta ->
          let converted = toLegacyConversationMetadata (setGct (Just GroupConversation) meta)
           in converted.cnvmGroupConvType === Just GroupConversationLegacy,
      testProperty "converts Channel -> ChannelLegacy" $
        \meta ->
          let converted = toLegacyConversationMetadata (setGct (Just Channel) meta)
           in converted.cnvmGroupConvType === Just ChannelLegacy,
      testProperty "drops MeetingConversation to Nothing" $
        \meta ->
          let converted = toLegacyConversationMetadata (setGct (Just MeetingConversation) meta)
           in converted.cnvmGroupConvType === Nothing,
      testProperty "preserves all fields except group conv type" $
        \meta ->
          let withGct = setGct meta.cnvmGroupConvType meta
              converted = toLegacyConversationMetadata withGct
           in (converted {cnvmGroupConvType = withGct.cnvmGroupConvType} === withGct)
    ]

testToLegacyOwnConversation :: TestTree
testToLegacyOwnConversation =
  testProperty "converts metadata, preserves other fields" $
    \(conv :: OwnConversation GroupConvType) ->
      let conv' = toLegacyOwnConversation conv
       in conv'.cnvQualifiedId === conv.cnvQualifiedId
            .&&. conv'.cnvMembers === conv.cnvMembers
            .&&. conv'.cnvProtocol === conv.cnvProtocol
            .&&. conv'.cnvMetadata.cnvmGroupConvType === (conv.cnvMetadata.cnvmGroupConvType >>= toGroupConvTypeLegacy)

testToLegacyConversation :: TestTree
testToLegacyConversation =
  testProperty "converts metadata, preserves other fields" $
    \(conv :: Conversation GroupConvType) ->
      let conv' = toLegacyConversation conv
       in conv'.qualifiedId === conv.qualifiedId
            .&&. conv'.members === conv.members
            .&&. conv'.protocol === conv.protocol
            .&&. conv'.metadata.cnvmGroupConvType === (conv.metadata.cnvmGroupConvType >>= toGroupConvTypeLegacy)

testToLegacyConversationsResponse :: TestTree
testToLegacyConversationsResponse =
  testProperty "drops meeting conversations from found, preserves the rest" $
    \(convs :: [OwnConversation GroupConvType]) ->
      let resp = toLegacyConversationsResponse (ConversationsResponse convs [] [])
          foundIds = map (.cnvQualifiedId) resp.crFound
          expectedFound = map (.cnvQualifiedId) (filter (not . isMeetingConversation) convs)
       in foundIds === expectedFound
            .&&. resp.crNotFound === []
            .&&. resp.crFailed === []

testIsMeetingConversation :: TestTree
testIsMeetingConversation =
  testProperty "matches Just MeetingConversation" $
    \(conv :: OwnConversation GroupConvType) gct ->
      isMeetingConversation (conv {cnvMetadata = conv.cnvMetadata {cnvmGroupConvType = gct}})
        === (gct == Just MeetingConversation)

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
{-# LANGUAGE StandaloneKindSignatures #-}
-- Ignore unused `genSingletons` Template Haskell results
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Wire.API.Conversation.Action
  ( ConversationAction,
    ConversationActionTag (..),
    SConversationActionTag (..),
    SomeConversationAction (..),
    conversationActionToEvent,
    conversationActionPermission,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.:), (.=))
import Data.Id
import Data.Qualified (Qualified)
import Data.Schema (ToSchema, element, enum, schema, schemaParseJSON, schemaToJSON)
import Data.Singletons (Sing, SomeSing (SomeSing), fromSing, toSing)
import Data.Singletons.TH (genSingletons, sCases)
import Data.Time.Clock
import Imports
import Test.QuickCheck (elements)
import Wire.API.Arbitrary (Arbitrary (..))
import Wire.API.Conversation
import Wire.API.Conversation.Role
import Wire.API.Event.Conversation

data ConversationActionTag
  = ConversationJoinTag
  | ConversationLeaveTag
  | ConversationRemoveMembersTag
  | ConversationMemberUpdateTag
  | ConversationDeleteTag
  | ConversationRenameTag
  | ConversationMessageTimerUpdateTag
  | ConversationReceiptModeUpdateTag
  | ConversationAccessDataTag
  deriving (Show, Eq, Generic, Bounded, Enum)

instance Arbitrary ConversationActionTag where
  arbitrary = elements [minBound .. maxBound]

instance ToSchema ConversationActionTag where
  schema =
    enum @Text "ConversationActionTag" $
      mconcat
        [ element "ConversationJoinTag" ConversationJoinTag,
          element "ConversationLeaveTag" ConversationLeaveTag,
          element "ConversationRemoveMembersTag" ConversationRemoveMembersTag,
          element "ConversationMemberUpdateTag" ConversationMemberUpdateTag,
          element "ConversationDeleteTag" ConversationDeleteTag,
          element "ConversationRenameTag" ConversationRenameTag,
          element "ConversationMessageTimerUpdateTag" ConversationMessageTimerUpdateTag,
          element "ConversationReceiptModeUpdateTag" ConversationReceiptModeUpdateTag,
          element "ConversationAccessDataTag" ConversationAccessDataTag
        ]

instance ToJSON ConversationActionTag where
  toJSON = schemaToJSON

instance FromJSON ConversationActionTag where
  parseJSON = schemaParseJSON

$(genSingletons [''ConversationActionTag])

-- | We use this type family instead of a sum type to be able to define
-- individual effects per conversation action. See 'HasConversationActionEffects'.
type family ConversationAction (tag :: ConversationActionTag) :: * where
  ConversationAction 'ConversationJoinTag = ConversationJoin
  ConversationAction 'ConversationLeaveTag = ConversationLeave
  ConversationAction 'ConversationMemberUpdateTag = ConversationMemberUpdate
  ConversationAction 'ConversationDeleteTag = ConversationDelete
  ConversationAction 'ConversationRenameTag = ConversationRename
  ConversationAction 'ConversationMessageTimerUpdateTag = ConversationMessageTimerUpdate
  ConversationAction 'ConversationReceiptModeUpdateTag = ConversationReceiptModeUpdate
  ConversationAction 'ConversationAccessDataTag = ConversationAccessData
  ConversationAction 'ConversationRemoveMembersTag = ConversationRemoveMembers

data SomeConversationAction where
  SomeConversationAction :: Sing tag -> ConversationAction tag -> SomeConversationAction

instance Show SomeConversationAction where
  show (SomeConversationAction tag action) =
    $(sCases ''ConversationActionTag [|tag|] [|show action|])

instance Eq SomeConversationAction where
  (SomeConversationAction tag1 action1) == (SomeConversationAction tag2 action2) =
    case (tag1, tag2) of
      (SConversationJoinTag, SConversationJoinTag) -> action1 == action2
      (SConversationJoinTag, _) -> False
      (SConversationLeaveTag, SConversationLeaveTag) -> action1 == action2
      (SConversationLeaveTag, _) -> False
      (SConversationMemberUpdateTag, SConversationMemberUpdateTag) -> action1 == action2
      (SConversationMemberUpdateTag, _) -> False
      (SConversationDeleteTag, SConversationDeleteTag) -> action1 == action2
      (SConversationDeleteTag, _) -> False
      (SConversationRenameTag, SConversationRenameTag) -> action1 == action2
      (SConversationRenameTag, _) -> False
      (SConversationMessageTimerUpdateTag, SConversationMessageTimerUpdateTag) -> action1 == action2
      (SConversationMessageTimerUpdateTag, _) -> False
      (SConversationReceiptModeUpdateTag, SConversationReceiptModeUpdateTag) -> action1 == action2
      (SConversationReceiptModeUpdateTag, _) -> False
      (SConversationAccessDataTag, SConversationAccessDataTag) -> action1 == action2
      (SConversationAccessDataTag, _) -> False
      (SConversationRemoveMembersTag, SConversationRemoveMembersTag) -> action1 == action2
      (SConversationRemoveMembersTag, _) -> False

instance ToJSON SomeConversationAction where
  toJSON (SomeConversationAction sb action) =
    let tag = fromSing sb
        actionJSON :: Value =
          $(sCases ''ConversationActionTag [|sb|] [|toJSON action|])
     in object ["tag" .= tag, "action" .= actionJSON]

instance FromJSON SomeConversationAction where
  parseJSON = withObject "SomeConversationAction" $ \ob -> do
    tag :: ConversationActionTag <- ob .: "tag"
    case toSing tag of
      SomeSing sb -> do
        $(sCases ''ConversationActionTag [|sb|] [|SomeConversationAction sb <$> (ob .: "action")|])

instance Arbitrary SomeConversationAction where
  arbitrary = do
    tag <- arbitrary
    case toSing tag of
      SomeSing sb -> do
        $(sCases ''ConversationActionTag [|sb|] [|SomeConversationAction sb <$> arbitrary|])

conversationActionPermission :: ConversationActionTag -> Action
conversationActionPermission ConversationJoinTag = AddConversationMember
conversationActionPermission ConversationLeaveTag = LeaveConversation
conversationActionPermission ConversationRemoveMembersTag = RemoveConversationMember
conversationActionPermission ConversationMemberUpdateTag = ModifyOtherConversationMember
conversationActionPermission ConversationDeleteTag = DeleteConversation
conversationActionPermission ConversationRenameTag = ModifyConversationName
conversationActionPermission ConversationMessageTimerUpdateTag = ModifyConversationMessageTimer
conversationActionPermission ConversationReceiptModeUpdateTag = ModifyConversationReceiptMode
conversationActionPermission ConversationAccessDataTag = ModifyConversationAccess

conversationActionToEvent ::
  forall tag.
  Sing tag ->
  UTCTime ->
  Qualified UserId ->
  Qualified ConvId ->
  ConversationAction tag ->
  Event
-- conversationActionToEvent now quid qcnv (ConversationActionAddMembers newMembers role) =
--   Event qcnv quid now $
--     EdMembersJoin $ SimpleMembers (map (`SimpleMember` role) (toList newMembers))
-- conversationActionToEvent now quid qcnv (ConversationActionRemoveMembers removedMembers) =
--   Event qcnv quid now $
--     EdMembersLeave (QualifiedUserIdList (toList removedMembers))
-- conversationActionToEvent now quid qcnv (ConversationActionRename rename) =
--   Event qcnv quid now (EdConvRename rename)
-- conversationActionToEvent now quid qcnv (ConversationActionMessageTimerUpdate update) =
--   Event qcnv quid now (EdConvMessageTimerUpdate update)
-- conversationActionToEvent now quid qcnv (ConversationActionReceiptModeUpdate update) =
--   Event qcnv quid now (EdConvReceiptModeUpdate update)
-- conversationActionToEvent now quid qcnv (ConversationActionMemberUpdate target (OtherMemberUpdate role)) =
--   let update = MemberUpdateData target Nothing Nothing Nothing Nothing Nothing Nothing role
--    in Event qcnv quid now (EdMemberUpdate update)
-- conversationActionToEvent now quid qcnv (ConversationActionAccessUpdate update) =
--   Event qcnv quid now (EdConvAccessUpdate update)
-- conversationActionToEvent now quid qcnv ConversationActionDelete =
--   Event qcnv quid now EdConvDelete
-- 
-- conversationActionTag :: Qualified UserId -> ConversationAction -> Action
-- conversationActionTag _ (ConversationActionAddMembers _ _) = AddConversationMember
-- conversationActionTag qusr (ConversationActionRemoveMembers victims)
--   | pure qusr == victims = LeaveConversation
--   | otherwise = RemoveConversationMember
-- conversationActionTag _ (ConversationActionRename _) = ModifyConversationName
-- conversationActionTag _ (ConversationActionMessageTimerUpdate _) = ModifyConversationMessageTimer
-- conversationActionTag _ (ConversationActionReceiptModeUpdate _) = ModifyConversationReceiptMode
-- conversationActionTag _ (ConversationActionMemberUpdate _ _) = ModifyOtherConversationMember
-- conversationActionTag _ (ConversationActionAccessUpdate _) = ModifyConversationAccess
-- conversationActionTag _ ConversationActionDelete = DeleteConversation
conversationActionToEvent tag now quid qcnv action =
  case tag of
    SConversationJoinTag ->
      let ConversationJoin newMembers role = action
       in Event qcnv quid now $
            EdMembersJoin $ SimpleMembers (map (`SimpleMember` role) (toList newMembers))
    SConversationLeaveTag ->
      let ConversationLeave leavingMembers = action
       in Event qcnv quid now $
            EdMembersLeave (QualifiedUserIdList (toList leavingMembers))
    SConversationRemoveMembersTag ->
      let ConversationRemoveMembers targets = action
       in Event qcnv quid now $
            EdMembersLeave (QualifiedUserIdList (toList targets))
    SConversationMemberUpdateTag ->
      let ConversationMemberUpdate target (OtherMemberUpdate role) = action
          update = MemberUpdateData target Nothing Nothing Nothing Nothing Nothing Nothing role
       in Event qcnv quid now (EdMemberUpdate update)
    SConversationDeleteTag ->
      Event qcnv quid now EdConvDelete
    SConversationRenameTag ->
      Event qcnv quid now $ EdConvRename action
    SConversationMessageTimerUpdateTag ->
      Event qcnv quid now (EdConvMessageTimerUpdate action)
    SConversationReceiptModeUpdateTag ->
      Event qcnv quid now (EdConvReceiptModeUpdate action)
    SConversationAccessDataTag ->
      Event qcnv quid now (EdConvAccessUpdate action)

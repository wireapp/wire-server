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
{-# LANGUAGE TemplateHaskell #-}
-- Ignore unused `genSingletons` Template Haskell results
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Wire.API.Conversation.Action
  ( ConversationAction,
    ConversationActionTag (..),
    SConversationActionTag (..),
    SomeConversationAction (..),
    conversationActionToEvent,
    conversationActionPermission,
    ConversationActionPermission,
    sConversationActionPermission,
  )
where

import Control.Lens ((?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as A
import Data.Id
import qualified Data.List.NonEmpty as NonEmptyList
import Data.Qualified (Qualified)
import Data.Schema hiding (tag)
import Data.Singletons.TH
import qualified Data.Swagger as S
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

$(singDecideInstance ''ConversationActionTag)

-- | We use this type family instead of a sum type to be able to define
-- individual effects per conversation action. See 'HasConversationActionEffects'.
type family ConversationAction (tag :: ConversationActionTag) :: * where
  ConversationAction 'ConversationJoinTag = ConversationJoin
  ConversationAction 'ConversationLeaveTag = NonEmptyList.NonEmpty (Qualified UserId)
  ConversationAction 'ConversationMemberUpdateTag = ConversationMemberUpdate
  ConversationAction 'ConversationDeleteTag = ()
  ConversationAction 'ConversationRenameTag = ConversationRename
  ConversationAction 'ConversationMessageTimerUpdateTag = ConversationMessageTimerUpdate
  ConversationAction 'ConversationReceiptModeUpdateTag = ConversationReceiptModeUpdate
  ConversationAction 'ConversationAccessDataTag = ConversationAccessData
  ConversationAction 'ConversationRemoveMembersTag = NonEmptyList.NonEmpty (Qualified UserId)

data SomeConversationAction where
  SomeConversationAction :: Sing tag -> ConversationAction tag -> SomeConversationAction

instance Show SomeConversationAction where
  show (SomeConversationAction tag action) =
    $(sCases ''ConversationActionTag [|tag|] [|show action|])

instance Eq SomeConversationAction where
  (SomeConversationAction tag1 action1) == (SomeConversationAction tag2 action2) =
    case tag1 %~ tag2 of
      Proved Refl -> $(sCases ''ConversationActionTag [|tag1|] [|action1 == action2|])
      Disproved _ -> False

instance ToJSON SomeConversationAction where
  toJSON (SomeConversationAction sb action) =
    let tag = fromSing sb
        actionJSON = fromMaybe A.Null $ schemaOut (conversationActionSchema sb) action
     in A.object ["tag" A..= tag, "action" A..= actionJSON]

conversationActionSchema :: forall tag. Sing tag -> ValueSchema NamedSwaggerDoc (ConversationAction tag)
conversationActionSchema SConversationJoinTag = schema @ConversationJoin
conversationActionSchema SConversationLeaveTag =
  objectWithDocModifier
    "ConversationLeave"
    (S.description ?~ "The action of some users leaving a conversation on their own")
    $ field "users" (nonEmptyArray schema)
conversationActionSchema SConversationRemoveMembersTag =
  objectWithDocModifier
    "ConversationRemoveMembers"
    (S.description ?~ "The action of some users being removed from a conversation")
    $ field "targets" (nonEmptyArray schema)
conversationActionSchema SConversationMemberUpdateTag = schema @ConversationMemberUpdate
conversationActionSchema SConversationDeleteTag =
  objectWithDocModifier
    "ConversationDelete"
    (S.description ?~ "The action of deleting a conversation")
    (pure ())
conversationActionSchema SConversationRenameTag = schema
conversationActionSchema SConversationMessageTimerUpdateTag = schema
conversationActionSchema SConversationReceiptModeUpdateTag = schema
conversationActionSchema SConversationAccessDataTag = schema

instance FromJSON SomeConversationAction where
  parseJSON = A.withObject "SomeConversationAction" $ \ob -> do
    tag <- ob A..: "tag"
    case A.lookup "action" ob of
      Nothing -> fail "'action' property missing"
      Just actionValue ->
        case toSing tag of
          SomeSing sb -> do
            action <- schemaIn (conversationActionSchema sb) actionValue
            return $ SomeConversationAction sb action

instance Arbitrary SomeConversationAction where
  arbitrary = do
    tag <- arbitrary
    case toSing tag of
      SomeSing sb -> do
        $(sCases ''ConversationActionTag [|sb|] [|SomeConversationAction sb <$> arbitrary|])

$( singletons
     [d|
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
       |]
 )

conversationActionToEvent ::
  forall tag.
  Sing tag ->
  UTCTime ->
  Qualified UserId ->
  Qualified ConvId ->
  ConversationAction tag ->
  Event
conversationActionToEvent tag now quid qcnv action =
  let edata = case tag of
        SConversationJoinTag ->
          let ConversationJoin newMembers role = action
           in EdMembersJoin $ SimpleMembers (map (`SimpleMember` role) (toList newMembers))
        SConversationLeaveTag ->
          EdMembersLeave (QualifiedUserIdList (toList action))
        SConversationRemoveMembersTag ->
          EdMembersLeave (QualifiedUserIdList (toList action))
        SConversationMemberUpdateTag ->
          let ConversationMemberUpdate target (OtherMemberUpdate role) = action
              update = MemberUpdateData target Nothing Nothing Nothing Nothing Nothing Nothing role
           in EdMemberUpdate update
        SConversationDeleteTag -> EdConvDelete
        SConversationRenameTag -> EdConvRename action
        SConversationMessageTimerUpdateTag -> EdConvMessageTimerUpdate action
        SConversationReceiptModeUpdateTag -> EdConvReceiptModeUpdate action
        SConversationAccessDataTag -> EdConvAccessUpdate action
   in Event qcnv quid now edata

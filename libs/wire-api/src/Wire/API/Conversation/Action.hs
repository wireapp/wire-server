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
    protocolValidAction,
  )
where

import Control.Lens hiding ((%~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as A
import Data.Id
import Data.Kind
import Data.OpenApi qualified as S
import Data.Qualified (Qualified)
import Data.Schema hiding (tag)
import Data.Singletons.TH
import Data.Time.Clock
import Imports
import Wire.API.Conversation
import Wire.API.Conversation.Action.Tag
import Wire.API.Conversation.Protocol hiding (protocolTag)
import Wire.API.Conversation.Role
import Wire.API.Event.Conversation
import Wire.API.Event.LeaveReason
import Wire.API.MLS.SubConversation
import Wire.API.Routes.Public.Galley.MLS
import Wire.Arbitrary (Arbitrary (..))

-- | We use this type family instead of a sum type to be able to define
-- individual effects per conversation action. See 'HasConversationActionEffects'.
type family ConversationAction (tag :: ConversationActionTag) :: Type where
  ConversationAction ConversationJoinTag = ConversationJoin
  ConversationAction ConversationLeaveTag = ()
  ConversationAction ConversationMemberUpdateTag = ConversationMemberUpdate
  ConversationAction ConversationDeleteTag = ()
  ConversationAction ConversationRenameTag = ConversationRename
  ConversationAction ConversationMessageTimerUpdateTag = ConversationMessageTimerUpdate
  ConversationAction ConversationReceiptModeUpdateTag = ConversationReceiptModeUpdate
  ConversationAction ConversationAccessDataTag = ConversationAccessData
  ConversationAction ConversationRemoveMembersTag = ConversationRemoveMembers
  ConversationAction ConversationUpdateProtocolTag = ProtocolTag
  ConversationAction ConversationUpdateAddPermissionTag = AddPermissionUpdate
  ConversationAction ConversationResetTag = MLSReset

data SomeConversationAction where
  SomeConversationAction :: Sing tag -> ConversationAction tag -> SomeConversationAction

instance Show SomeConversationAction where
  show (SomeConversationAction tag action) =
    "SomeConversationAction {tag = "
      <> show (fromSing tag)
      <> ", action = "
      <> $(sCases ''ConversationActionTag [|tag|] [|show action|])
      <> "}"

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

instance S.ToSchema SomeConversationAction where
  declareNamedSchema _ = do
    unitSchema <- S.declareSchemaRef (Proxy :: Proxy ())
    conversationJoin <- S.declareSchemaRef (Proxy :: Proxy ConversationJoin)
    conversationMemberUpdate <- S.declareSchemaRef (Proxy :: Proxy ConversationMemberUpdate)
    conversationRename <- S.declareSchemaRef (Proxy :: Proxy ConversationRename)
    conversationMessageTimerUpdate <- S.declareSchemaRef (Proxy :: Proxy ConversationMessageTimerUpdate)
    conversationReceiptModeUpdate <- S.declareSchemaRef (Proxy :: Proxy ConversationReceiptModeUpdate)
    conversationAccessData <- S.declareSchemaRef (Proxy :: Proxy ConversationAccessData)
    conversationRemoveMembers <- S.declareSchemaRef (Proxy :: Proxy ConversationRemoveMembers)
    protocolTag <- S.declareSchemaRef (Proxy :: Proxy ProtocolTag)
    let schemas =
          [ (toJSON ConversationJoinTag, conversationJoin),
            (toJSON ConversationLeaveTag, unitSchema),
            (toJSON ConversationMemberUpdateTag, conversationMemberUpdate),
            (toJSON ConversationDeleteTag, unitSchema),
            (toJSON ConversationRenameTag, conversationRename),
            (toJSON ConversationMessageTimerUpdateTag, conversationMessageTimerUpdate),
            (toJSON ConversationReceiptModeUpdateTag, conversationReceiptModeUpdate),
            (toJSON ConversationAccessDataTag, conversationAccessData),
            (toJSON ConversationRemoveMembersTag, conversationRemoveMembers),
            (toJSON ConversationUpdateProtocolTag, protocolTag)
          ]
            <&> \(t, a) ->
              S.Inline $
                mempty
                  & S.type_ ?~ S.OpenApiObject
                  & S.properties . at "tag" ?~ S.Inline (mempty & S.type_ ?~ S.OpenApiString & S.enum_ ?~ [t])
                  & S.properties . at "action" ?~ a
                  & S.required .~ ["tag", "action"]
    pure $
      S.NamedSchema (Just "SomeConversationAction") $
        mempty & S.oneOf ?~ schemas

conversationActionSchema :: forall tag. Sing tag -> ValueSchema NamedSwaggerDoc (ConversationAction tag)
conversationActionSchema SConversationJoinTag = schema @ConversationJoin
conversationActionSchema SConversationLeaveTag =
  objectWithDocModifier
    "ConversationLeave"
    (S.description ?~ "The action of some users leaving a conversation on their own")
    $ pure ()
conversationActionSchema SConversationRemoveMembersTag = schema
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
conversationActionSchema SConversationUpdateProtocolTag = schema
conversationActionSchema SConversationUpdateAddPermissionTag = schema
conversationActionSchema SConversationResetTag = schema

instance FromJSON SomeConversationAction where
  parseJSON = A.withObject "SomeConversationAction" $ \ob -> do
    tag <- ob A..: "tag"
    case A.lookup "action" ob of
      Nothing -> fail "'action' property missing"
      Just actionValue ->
        case toSing tag of
          SomeSing sb -> do
            action <- schemaIn (conversationActionSchema sb) actionValue
            pure $ SomeConversationAction sb action

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
       conversationActionPermission ConversationUpdateProtocolTag = LeaveConversation
       conversationActionPermission ConversationUpdateAddPermissionTag = ModifyAddPermission
       conversationActionPermission ConversationResetTag = LeaveConversation
       |]
 )

conversationActionToEvent ::
  forall tag.
  Sing tag ->
  UTCTime ->
  Qualified UserId ->
  Qualified ConvId ->
  Maybe SubConvId ->
  ConversationAction tag ->
  Event
conversationActionToEvent tag now quid qcnv subconv action =
  let edata = case tag of
        SConversationJoinTag ->
          let ConversationJoin newMembers role joinType = action
           in EdMembersJoin $ MembersJoin (map (`SimpleMember` role) (toList newMembers)) joinType
        SConversationLeaveTag ->
          EdMembersLeave EdReasonLeft (QualifiedUserIdList [quid])
        SConversationRemoveMembersTag ->
          EdMembersLeave (crmReason action) (QualifiedUserIdList . toList . crmTargets $ action)
        SConversationMemberUpdateTag ->
          let ConversationMemberUpdate target (OtherMemberUpdate role) = action
              update = MemberUpdateData target Nothing Nothing Nothing Nothing Nothing Nothing role
           in EdMemberUpdate update
        SConversationDeleteTag -> EdConvDelete
        SConversationRenameTag -> EdConvRename action
        SConversationMessageTimerUpdateTag -> EdConvMessageTimerUpdate action
        SConversationReceiptModeUpdateTag -> EdConvReceiptModeUpdate action
        SConversationAccessDataTag -> EdConvAccessUpdate action
        SConversationUpdateProtocolTag -> EdProtocolUpdate action
        SConversationUpdateAddPermissionTag -> EdAddPermissionUpdate action
        SConversationResetTag -> EdConvReset action.groupId
   in Event qcnv subconv quid now edata

-- | Certain actions need to be performed at the level of the underlying
-- protocol (MLS, mostly) before being applied to conversations. This function
-- returns whether a given action tag is directly applicable to a conversation
-- with the given protocol.
protocolValidAction :: Protocol -> Sing tag -> ConversationAction tag -> Bool
protocolValidAction ProtocolProteus _ _ = True
protocolValidAction (ProtocolMixed _) _ _ = True
protocolValidAction (ProtocolMLS _) SConversationJoinTag (ConversationJoin _ _ InternalAdd) = False
protocolValidAction (ProtocolMLS _) SConversationJoinTag (ConversationJoin _ _ ExternalAdd) = True
protocolValidAction (ProtocolMLS _) SConversationLeaveTag _ = True
protocolValidAction (ProtocolMLS _) SConversationRemoveMembersTag _ = False
protocolValidAction (ProtocolMLS _) SConversationDeleteTag _ = True
protocolValidAction (ProtocolMLS _) _ _ = True

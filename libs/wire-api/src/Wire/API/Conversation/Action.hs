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

module Wire.API.Conversation.Action
  ( ConversationAction (..),
    conversationActionToEvent,
    conversationActionTag,
  )
where

import Control.Lens.Combinators
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Id
import Data.List.NonEmpty (NonEmpty)
import Data.Qualified
import Data.Schema
import qualified Data.Swagger as S
import Data.Time.Clock
import Imports
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))
import Wire.API.Conversation
import Wire.API.Conversation.Role
import Wire.API.Event.Conversation
import Wire.API.Util.Aeson (CustomEncoded (..))

-- | A sum type consisting of all possible conversation actions.
data ConversationAction
  = ConversationActionAddMembers (NonEmpty (Qualified UserId)) RoleName
  | ConversationActionRemoveMembers (NonEmpty (Qualified UserId))
  | ConversationActionRename ConversationRename
  | ConversationActionMessageTimerUpdate ConversationMessageTimerUpdate
  | ConversationActionReceiptModeUpdate ConversationReceiptModeUpdate
  | ConversationActionMemberUpdate (Qualified UserId) OtherMemberUpdate
  | ConversationActionAccessUpdate ConversationAccessData
  | ConversationActionDelete
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConversationAction)
  deriving (ToJSON, FromJSON) via (CustomEncoded ConversationAction)
  deriving (S.ToSchema) via (Schema ConversationAction)

instance ToSchema ConversationAction where
  schema =
    named "ConversationAction" $
      mconcat
        [ tag _ConversationActionAddMembers (unnamed roleNameSchema),
          tag _ConversationActionRemoveMembers (nonEmptyArray $ schema @(Qualified UserId)),
          tag _ConversationActionRename (unnamed schema),
          tag _ConversationActionMessageTimerUpdate (unnamed schema),
          tag _ConversationActionReceiptModeUpdate (unnamed schema),
          tag _ConversationActionMemberUpdate (unnamed memberUpdateSchema),
          tag _ConversationActionAccessUpdate (unnamed schema),
          tag _ConversationActionDelete null_
        ]
    where
      roleNameSchema :: ValueSchema NamedSwaggerDoc (NonEmpty (Qualified UserId), RoleName)
      roleNameSchema =
        object "AddMembers" $
          (,)
            <$> fst .= field "members" (nonEmptyArray schema)
            <*> snd .= field "role_name" schema
      memberUpdateSchema :: ValueSchema NamedSwaggerDoc (Qualified UserId, OtherMemberUpdate)
      memberUpdateSchema =
        object "MemberUpdate" $
          (,)
            <$> fst .= field "qualified_user_id" schema
            <*> snd .= field "other_member_update" schema

conversationActionToEvent ::
  UTCTime ->
  Qualified UserId ->
  Qualified ConvId ->
  ConversationAction ->
  Event
conversationActionToEvent now quid qcnv (ConversationActionAddMembers newMembers role) =
  Event MemberJoin qcnv quid now $
    EdMembersJoin $ SimpleMembers (map (`SimpleMember` role) (toList newMembers))
conversationActionToEvent now quid qcnv (ConversationActionRemoveMembers removedMembers) =
  Event MemberLeave qcnv quid now $
    EdMembersLeave (QualifiedUserIdList (toList removedMembers))
conversationActionToEvent now quid qcnv (ConversationActionRename rename) =
  Event ConvRename qcnv quid now (EdConvRename rename)
conversationActionToEvent now quid qcnv (ConversationActionMessageTimerUpdate update) =
  Event ConvMessageTimerUpdate qcnv quid now (EdConvMessageTimerUpdate update)
conversationActionToEvent now quid qcnv (ConversationActionReceiptModeUpdate update) =
  Event ConvReceiptModeUpdate qcnv quid now (EdConvReceiptModeUpdate update)
conversationActionToEvent now quid qcnv (ConversationActionMemberUpdate target (OtherMemberUpdate role)) =
  let update = MemberUpdateData target Nothing Nothing Nothing Nothing Nothing Nothing role
   in Event MemberStateUpdate qcnv quid now (EdMemberUpdate update)
conversationActionToEvent now quid qcnv (ConversationActionAccessUpdate update) =
  Event ConvAccessUpdate qcnv quid now (EdConvAccessUpdate update)
conversationActionToEvent now quid qcnv ConversationActionDelete =
  Event ConvDelete qcnv quid now EdConvDelete

conversationActionTag :: Qualified UserId -> ConversationAction -> Action
conversationActionTag _ (ConversationActionAddMembers _ _) = AddConversationMember
conversationActionTag qusr (ConversationActionRemoveMembers victims)
  | pure qusr == victims = LeaveConversation
  | otherwise = RemoveConversationMember
conversationActionTag _ (ConversationActionRename _) = ModifyConversationName
conversationActionTag _ (ConversationActionMessageTimerUpdate _) = ModifyConversationMessageTimer
conversationActionTag _ (ConversationActionReceiptModeUpdate _) = ModifyConversationReceiptMode
conversationActionTag _ (ConversationActionMemberUpdate _ _) = ModifyOtherConversationMember
conversationActionTag _ (ConversationActionAccessUpdate _) = ModifyConversationAccess
conversationActionTag _ ConversationActionDelete = DeleteConversation

-----------------------------------------------------------------------------
-- Prisms for the ToSchema instance for ConversationAction

_ConversationActionAddMembers ::
  Prism' ConversationAction (NonEmpty (Qualified UserId), RoleName)
_ConversationActionAddMembers =
  prism
    (uncurry ConversationActionAddMembers)
    ( \action ->
        case action of
          ConversationActionAddMembers uids role ->
            Right (uids, role)
          _ -> Left action
    )
{-# INLINE _ConversationActionAddMembers #-}

_ConversationActionRemoveMembers ::
  Prism' ConversationAction (NonEmpty (Qualified UserId))
_ConversationActionRemoveMembers =
  prism
    ConversationActionRemoveMembers
    ( \action ->
        case action of
          ConversationActionRemoveMembers uids -> Right uids
          _ -> Left action
    )
{-# INLINE _ConversationActionRemoveMembers #-}

_ConversationActionRename ::
  Prism' ConversationAction ConversationRename
_ConversationActionRename =
  prism
    ConversationActionRename
    ( \action ->
        case action of
          ConversationActionRename rename -> Right rename
          _ -> Left action
    )
{-# INLINE _ConversationActionRename #-}

_ConversationActionMessageTimerUpdate ::
  Prism' ConversationAction ConversationMessageTimerUpdate
_ConversationActionMessageTimerUpdate =
  prism
    ConversationActionMessageTimerUpdate
    ( \action ->
        case action of
          ConversationActionMessageTimerUpdate update -> Right update
          _ -> Left action
    )
{-# INLINE _ConversationActionMessageTimerUpdate #-}

_ConversationActionReceiptModeUpdate ::
  Prism' ConversationAction ConversationReceiptModeUpdate
_ConversationActionReceiptModeUpdate =
  prism
    ConversationActionReceiptModeUpdate
    ( \action ->
        case action of
          ConversationActionReceiptModeUpdate update -> Right update
          _ -> Left action
    )
{-# INLINE _ConversationActionReceiptModeUpdate #-}

_ConversationActionMemberUpdate ::
  Prism' ConversationAction (Qualified UserId, OtherMemberUpdate)
_ConversationActionMemberUpdate =
  prism
    (uncurry ConversationActionMemberUpdate)
    ( \action ->
        case action of
          ConversationActionMemberUpdate uid update ->
            Right (uid, update)
          _ -> Left action
    )
{-# INLINE _ConversationActionMemberUpdate #-}

_ConversationActionAccessUpdate ::
  Prism' ConversationAction ConversationAccessData
_ConversationActionAccessUpdate =
  prism
    ConversationActionAccessUpdate
    ( \action ->
        case action of
          ConversationActionAccessUpdate update -> Right update
          _ -> Left action
    )
{-# INLINE _ConversationActionAccessUpdate #-}

_ConversationActionDelete :: Prism' ConversationAction ()
_ConversationActionDelete =
  prism
    (const ConversationActionDelete)
    ( \action ->
        case action of
          ConversationActionDelete -> Right ()
          _ -> Left action
    )
{-# INLINE _ConversationActionDelete #-}

{-# LANGUAGE RecordWildCards #-}

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

module Galley.API.One2One
  ( one2OneConvId,
    iUpsertOne2OneConversation,
  )
where

import Data.Id
import Data.Qualified
import Galley.Data.Conversation
import Galley.Data.Conversation.Types
import Galley.Effects.ConversationStore
import Galley.Effects.MemberStore
import Galley.Types.Conversations.One2One (one2OneConvId)
import Galley.Types.ToUserRole
import Galley.Types.UserList
import Imports
import Polysemy
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.Protocol
import Wire.API.Routes.Internal.Galley.ConversationsIntra
import Wire.API.User

newConnectConversationWithRemote ::
  Local UserId ->
  UserList UserId ->
  NewConversation
newConnectConversationWithRemote creator users =
  NewConversation
    { ncMetadata =
        (defConversationMetadata (Just (tUnqualified creator)))
          { cnvmType = One2OneConv
          },
      ncUsers = fmap toUserRole users,
      ncProtocol = BaseProtocolProteusTag
    }

iUpsertOne2OneConversation ::
  forall r.
  ( Member ConversationStore r,
    Member MemberStore r
  ) =>
  UpsertOne2OneConversationRequest ->
  Sem r ()
iUpsertOne2OneConversation UpsertOne2OneConversationRequest {..} = do
  let dolocal :: Local ConvId -> Sem r ()
      dolocal lconvId = do
        mbConv <- getConversation (tUnqualified lconvId)
        case mbConv of
          Nothing -> do
            let members =
                  case (uooActor, uooActorDesiredMembership) of
                    (LocalActor, Included) -> ulFromLocals [tUnqualified uooLocalUser]
                    (LocalActor, Excluded) -> mempty
                    (RemoteActor, Included) -> ulFromRemotes [uooRemoteUser]
                    (RemoteActor, Excluded) -> mempty
            unless (null members) . void $
              createConversation
                lconvId
                (newConnectConversationWithRemote uooLocalUser members)
          Just conv -> do
            case (uooActor, uooActorDesiredMembership) of
              (LocalActor, Included) -> do
                void $ createMember lconvId uooLocalUser
                unless (null (convRemoteMembers conv)) $
                  acceptConnectConversation (tUnqualified lconvId)
              (LocalActor, Excluded) -> do
                deleteMembers
                  (tUnqualified lconvId)
                  (UserList [tUnqualified uooLocalUser] [])
                let mGroupId = case convProtocol conv of
                      ProtocolProteus -> Nothing
                      ProtocolMLS meta -> Just . cnvmlsGroupId $ meta
                      ProtocolMixed meta -> Just . cnvmlsGroupId $ meta
                for_ mGroupId $ flip removeAllMLSClientsOfUser (tUntagged uooLocalUser)
              (RemoteActor, Included) -> do
                void $ createMembers (tUnqualified lconvId) (UserList [] [uooRemoteUser])
                unless (null (convLocalMembers conv)) $
                  acceptConnectConversation (tUnqualified lconvId)
              (RemoteActor, Excluded) ->
                deleteMembers
                  (tUnqualified lconvId)
                  (UserList [] [uooRemoteUser])
      doremote :: Remote ConvId -> Sem r ()
      doremote rconvId =
        case (uooActor, uooActorDesiredMembership) of
          (LocalActor, Included) -> do
            createMembersInRemoteConversation rconvId [tUnqualified uooLocalUser]
          (LocalActor, Excluded) -> do
            deleteMembersInRemoteConversation rconvId [tUnqualified uooLocalUser]
          (RemoteActor, _) -> pure ()

  foldQualified uooLocalUser dolocal doremote uooConvId

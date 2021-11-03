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
{-# LANGUAGE RecordWildCards #-}

module Galley.API.One2One
  ( one2OneConvId,
    iUpsertOne2OneConversation,
  )
where

import Data.Id
import Data.Qualified
import Galley.App (Galley, liftSem)
import Galley.Data.Conversation
import Galley.Effects.ConversationStore
import Galley.Effects.MemberStore
import Galley.Types.Conversations.Intra (Actor (..), DesiredMembership (..), UpsertOne2OneConversationRequest (..), UpsertOne2OneConversationResponse (..))
import Galley.Types.Conversations.One2One (one2OneConvId)
import Galley.Types.UserList (UserList (..))
import Imports
import Polysemy

iUpsertOne2OneConversation ::
  forall r.
  Members '[ConversationStore, MemberStore] r =>
  UpsertOne2OneConversationRequest ->
  Galley r UpsertOne2OneConversationResponse
iUpsertOne2OneConversation UpsertOne2OneConversationRequest {..} = liftSem $ do
  let convId = fromMaybe (one2OneConvId (qUntagged uooLocalUser) (qUntagged uooRemoteUser)) uooConvId

  let dolocal :: Local ConvId -> Sem r ()
      dolocal lconvId = do
        mbConv <- getConversation (tUnqualified lconvId)
        case mbConv of
          Nothing -> do
            let members =
                  case (uooActor, uooActorDesiredMembership) of
                    (LocalActor, Included) -> UserList [tUnqualified uooLocalUser] []
                    (LocalActor, Excluded) -> UserList [] []
                    (RemoteActor, Included) -> UserList [] [uooRemoteUser]
                    (RemoteActor, Excluded) -> UserList [] []
            unless (null members) . void $
              createConnectConversationWithRemote
                (tUnqualified lconvId)
                (tUnqualified uooLocalUser)
                members
          Just conv -> do
            case (uooActor, uooActorDesiredMembership) of
              (LocalActor, Included) -> do
                void $ createMember lconvId uooLocalUser
                unless (null (convRemoteMembers conv)) $
                  acceptConnectConversation (tUnqualified lconvId)
              (LocalActor, Excluded) ->
                deleteMembers
                  (tUnqualified lconvId)
                  (UserList [tUnqualified uooLocalUser] [])
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

  foldQualified uooLocalUser dolocal doremote convId
  pure (UpsertOne2OneConversationResponse convId)

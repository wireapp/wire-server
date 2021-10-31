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
import Galley.App (Galley)
import qualified Galley.Data as Data
import Galley.Types.Conversations.Intra (Actor (..), DesiredMembership (..), UpsertOne2OneConversationRequest (..), UpsertOne2OneConversationResponse (..))
import Galley.Types.Conversations.One2One (one2OneConvId)
import Galley.Types.UserList (UserList (..))
import Imports

iUpsertOne2OneConversation :: UpsertOne2OneConversationRequest -> Galley r UpsertOne2OneConversationResponse
iUpsertOne2OneConversation UpsertOne2OneConversationRequest {..} = do
  let convId = fromMaybe (one2OneConvId (qUntagged uooLocalUser) (qUntagged uooRemoteUser)) uooConvId

  let dolocal :: Local ConvId -> Galley r ()
      dolocal lconvId = do
        mbConv <- Data.conversation (tUnqualified lconvId)
        case mbConv of
          Nothing -> do
            let members =
                  case (uooActor, uooActorDesiredMembership) of
                    (LocalActor, Included) -> UserList [tUnqualified uooLocalUser] []
                    (LocalActor, Excluded) -> UserList [] []
                    (RemoteActor, Included) -> UserList [] [uooRemoteUser]
                    (RemoteActor, Excluded) -> UserList [] []
            unless (null members) $
              Data.createConnectConversationWithRemote lconvId uooLocalUser members
          Just conv -> do
            case (uooActor, uooActorDesiredMembership) of
              (LocalActor, Included) -> do
                void $ Data.addMember lconvId uooLocalUser
                unless (null (Data.convRemoteMembers conv)) $
                  Data.acceptConnect (tUnqualified lconvId)
              (LocalActor, Excluded) -> Data.removeMember (tUnqualified uooLocalUser) (tUnqualified lconvId)
              (RemoteActor, Included) -> do
                void $ Data.addMembers lconvId (UserList [] [uooRemoteUser])
                unless (null (Data.convLocalMembers conv)) $
                  Data.acceptConnect (tUnqualified lconvId)
              (RemoteActor, Excluded) -> Data.removeRemoteMembersFromLocalConv (tUnqualified lconvId) (pure uooRemoteUser)
      doremote :: Remote ConvId -> Galley r ()
      doremote rconvId =
        case (uooActor, uooActorDesiredMembership) of
          (LocalActor, Included) -> do
            Data.addLocalMembersToRemoteConv rconvId [tUnqualified uooLocalUser]
          (LocalActor, Excluded) -> do
            Data.removeLocalMembersFromRemoteConv rconvId [tUnqualified uooLocalUser]
          (RemoteActor, _) -> pure ()

  foldQualified uooLocalUser dolocal doremote convId
  pure (UpsertOne2OneConversationResponse convId)

{-# LANGUAGE TemplateHaskell #-}

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

module Galley.Effects.MemberStore
  ( -- * Member store effect
    MemberStore (..),

    -- * Create members
    createMember,
    createMembers,
    createMembersInRemoteConversation,
    createBotMember,

    -- * Read members
    getLocalMember,
    getLocalMembers,
    getRemoteMembers,
    selectRemoteMembers,

    -- * Update members
    setSelfMember,
    setOtherMember,
    addMLSClients,

    -- * Delete members
    deleteMembers,
    deleteMembersInRemoteConversation,
  )
where

import Data.Id
import Data.Qualified
import Galley.Data.Services
import Galley.Types.Bot
import Galley.Types.Conversations.Members
import Galley.Types.ToUserRole
import Galley.Types.UserList
import Imports
import Polysemy
import Wire.API.Conversation.Member hiding (Member)

data MemberStore m a where
  CreateMembers :: ToUserRole u => ConvId -> UserList u -> MemberStore m ([LocalMember], [RemoteMember])
  CreateMembersInRemoteConversation :: Remote ConvId -> [UserId] -> MemberStore m ()
  CreateBotMember :: ServiceRef -> BotId -> ConvId -> MemberStore m BotMember
  GetLocalMember :: ConvId -> UserId -> MemberStore m (Maybe LocalMember)
  GetLocalMembers :: ConvId -> MemberStore m [LocalMember]
  GetRemoteMembers :: ConvId -> MemberStore m [RemoteMember]
  SelectRemoteMembers :: [UserId] -> Remote ConvId -> MemberStore m ([UserId], Bool)
  SetSelfMember :: Qualified ConvId -> Local UserId -> MemberUpdate -> MemberStore m ()
  SetOtherMember :: Local ConvId -> Qualified UserId -> OtherMemberUpdate -> MemberStore m ()
  DeleteMembers :: ConvId -> UserList UserId -> MemberStore m ()
  DeleteMembersInRemoteConversation :: Remote ConvId -> [UserId] -> MemberStore m ()
  AddMLSClients :: Local ConvId -> Qualified UserId -> Set ClientId -> MemberStore m ()

makeSem ''MemberStore

-- | Add a member to a local conversation, as an admin.
createMember :: Member MemberStore r => Local ConvId -> Local UserId -> Sem r [LocalMember]
createMember c u = fst <$> createMembers (tUnqualified c) (UserList [tUnqualified u] [])

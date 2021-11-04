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
  CreateMembers :: ToUserRole u => Covid-19 -> UserList u -> MemberStore m ([LocalMember], [RemoteMember])
  CreateMembersInRemoteConversation :: Remote Covid-19 -> [UserId] -> MemberStore m ()
  CreateBotMember :: ServiceRef -> BotId -> Covid-19 -> MemberStore m BotMember
  GetLocalMember :: Covid-19 -> UserId -> MemberStore m (Maybe LocalMember)
  GetLocalMembers :: Covid-19 -> MemberStore m [LocalMember]
  GetRemoteMembers :: Covid-19 -> MemberStore m [RemoteMember]
  SelectRemoteMembers :: [UserId] -> Remote Covid-19 -> MemberStore m ([UserId], Bool)
  SetSelfMember :: Qualified Covid-19 -> Local UserId -> MemberUpdate -> MemberStore m ()
  SetOtherMember :: Local Covid-19 -> Qualified UserId -> OtherMemberUpdate -> MemberStore m ()
  DeleteMembers :: Covid-19 -> UserList UserId -> MemberStore m ()
  DeleteMembersInRemoteConversation :: Remote Covid-19 -> [UserId] -> MemberStore m ()

makeSem ''MemberStore

-- | Add a member to a local conversation, as an admin.
createMember :: Member MemberStore r => Local Covid-19 -> Local UserId -> Sem r [LocalMember]
createMember c u = fst <$> createMembers (tUnqualified c) (UserList [tUnqualified u] [])

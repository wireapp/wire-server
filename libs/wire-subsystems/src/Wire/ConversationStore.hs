{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.ConversationStore where

import Data.Domain
import Data.Id
import Data.Misc
import Data.Qualified
import Data.Range
import Data.Time.Clock
import Imports
import Polysemy
import Wire.API.Conversation hiding (Conversation, Member)
import Wire.API.Conversation.CellsState
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Credential
import Wire.API.MLS.GroupInfo
import Wire.API.MLS.LeafNode
import Wire.API.MLS.SubConversation
import Wire.API.Provider.Service
import Wire.ConversationStore.MLS.Types
import Wire.StoredConversation
import Wire.UserList

data LockAcquired
  = Acquired
  | NotAcquired
  deriving (Show, Eq)

data MLSCommitLockStore m a where
  AcquireCommitLock :: GroupId -> Epoch -> NominalDiffTime -> MLSCommitLockStore m LockAcquired
  ReleaseCommitLock :: GroupId -> Epoch -> MLSCommitLockStore m ()

makeSem ''MLSCommitLockStore

data ConversationStore m a where
  CreateConversationId :: ConversationStore m ConvId
  CreateConversation :: Local ConvId -> NewConversation -> ConversationStore m StoredConversation
  CreateMLSSelfConversation ::
    Local UserId ->
    ConversationStore m StoredConversation
  DeleteConversation :: ConvId -> ConversationStore m ()
  GetConversation :: ConvId -> ConversationStore m (Maybe StoredConversation)
  GetConversationEpoch :: ConvId -> ConversationStore m (Maybe Epoch)
  GetConversations :: [ConvId] -> ConversationStore m [StoredConversation]
  GetConversationMetadata :: ConvId -> ConversationStore m (Maybe ConversationMetadata)
  GetGroupInfo :: ConvId -> ConversationStore m (Maybe GroupInfoData)
  IsConversationAlive :: ConvId -> ConversationStore m Bool
  GetRemoteConversationStatus ::
    UserId ->
    [Remote ConvId] ->
    ConversationStore m (Map (Remote ConvId) MemberStatus)
  SelectConversations :: UserId -> [ConvId] -> ConversationStore m [ConvId]
  SetConversationType :: ConvId -> ConvType -> ConversationStore m ()
  SetConversationName :: ConvId -> Range 1 256 Text -> ConversationStore m ()
  SetConversationAccess :: ConvId -> ConversationAccessData -> ConversationStore m ()
  SetConversationReceiptMode :: ConvId -> ReceiptMode -> ConversationStore m ()
  SetConversationMessageTimer :: ConvId -> Maybe Milliseconds -> ConversationStore m ()
  SetConversationEpoch :: ConvId -> Epoch -> ConversationStore m ()
  SetConversationCipherSuite :: ConvId -> CipherSuiteTag -> ConversationStore m ()
  SetConversationCellsState :: ConvId -> CellsState -> ConversationStore m ()
  ResetConversation :: ConvId -> GroupId -> ConversationStore m ()
  SetGroupInfo :: ConvId -> GroupInfoData -> ConversationStore m ()
  UpdateChannelAddPermissions :: ConvId -> AddPermission -> ConversationStore m ()
  UpdateToMixedProtocol :: Local ConvId -> ConvType -> ConversationStore m ()
  UpdateToMLSProtocol :: Local ConvId -> ConversationStore m ()
  DeleteTeamConversation :: TeamId -> ConvId -> ConversationStore m ()
  GetTeamConversation :: TeamId -> ConvId -> ConversationStore m (Maybe ConvId)
  GetTeamConversations :: TeamId -> ConversationStore m [ConvId]
  DeleteTeamConversations :: TeamId -> ConversationStore m ()
  -- MEMBER OPERATIONS
  CreateMembers :: (ToUserRole u) => ConvId -> UserList u -> ConversationStore m ([LocalMember], [RemoteMember])
  CreateMembersInRemoteConversation :: Remote ConvId -> [UserId] -> ConversationStore m ()
  CreateBotMember :: ServiceRef -> BotId -> ConvId -> ConversationStore m BotMember
  GetLocalMember :: ConvId -> UserId -> ConversationStore m (Maybe LocalMember)
  GetLocalMembers :: ConvId -> ConversationStore m [LocalMember]
  GetAllLocalMembers :: ConversationStore m [LocalMember]
  GetRemoteMember :: ConvId -> Remote UserId -> ConversationStore m (Maybe RemoteMember)
  GetRemoteMembers :: ConvId -> ConversationStore m [RemoteMember]
  CheckLocalMemberRemoteConv :: UserId -> Remote ConvId -> ConversationStore m Bool
  SelectRemoteMembers :: [UserId] -> Remote ConvId -> ConversationStore m ([UserId], Bool)
  SetSelfMember :: Qualified ConvId -> Local UserId -> MemberUpdate -> ConversationStore m ()
  SetOtherMember :: Local ConvId -> Qualified UserId -> OtherMemberUpdate -> ConversationStore m ()
  DeleteMembers :: ConvId -> UserList UserId -> ConversationStore m ()
  DeleteMembersInRemoteConversation :: Remote ConvId -> [UserId] -> ConversationStore m ()
  AddMLSClients :: GroupId -> Qualified UserId -> Set (ClientId, LeafIndex) -> ConversationStore m ()
  PlanClientRemoval :: (Foldable f) => GroupId -> f ClientIdentity -> ConversationStore m ()
  RemoveMLSClients :: GroupId -> Qualified UserId -> Set ClientId -> ConversationStore m ()
  RemoveAllMLSClients :: GroupId -> ConversationStore m ()
  LookupMLSClients :: GroupId -> ConversationStore m (ClientMap LeafIndex)
  LookupMLSClientLeafIndices :: GroupId -> ConversationStore m (ClientMap LeafIndex, IndexMap)
  GetRemoteMembersByDomain :: Domain -> ConversationStore m [(ConvId, RemoteMember)]
  GetLocalMembersByDomain :: Domain -> ConversationStore m [(ConvId, UserId)]
  -- SUB CONVERSATION OPERATIONS
  CreateSubConversation :: ConvId -> SubConvId -> GroupId -> ConversationStore m SubConversation
  GetSubConversation :: ConvId -> SubConvId -> ConversationStore m (Maybe SubConversation)
  GetSubConversationGroupInfo :: ConvId -> SubConvId -> ConversationStore m (Maybe GroupInfoData)
  GetSubConversationEpoch :: ConvId -> SubConvId -> ConversationStore m (Maybe Epoch)
  SetSubConversationGroupInfo :: ConvId -> SubConvId -> Maybe GroupInfoData -> ConversationStore m ()
  SetSubConversationEpoch :: ConvId -> SubConvId -> Epoch -> ConversationStore m ()
  SetSubConversationCipherSuite :: ConvId -> SubConvId -> CipherSuiteTag -> ConversationStore m ()
  ListSubConversations :: ConvId -> ConversationStore m (Map SubConvId ConversationMLSData)
  DeleteSubConversation :: ConvId -> SubConvId -> ConversationStore m ()

makeSem ''ConversationStore

acceptConnectConversation :: (Member ConversationStore r) => ConvId -> Sem r ()
acceptConnectConversation cid = setConversationType cid One2OneConv

-- | Add a member to a local conversation, as an admin.
createMember :: (Member ConversationStore r) => Local ConvId -> Local UserId -> Sem r [LocalMember]
createMember c u = fst <$> createMembers (tUnqualified c) (UserList [tUnqualified u] [])

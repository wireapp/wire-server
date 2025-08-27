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

-- TODO(leif): make sure these are only used in the store interpreter, maybe inline them?
module Wire.ConversationStore.Cassandra.Queries
  ( insertMLSSelfConv,
    insertMember,
    insertUserConv,
    insertRemoteMember,
    removeMember,
    deleteUserConv,
    deleteUserRemoteConv,
    removeMLSClient,
    removeAllMLSClients,
    removeRemoteMember,
    selectMember,
    selectMembers,
    selectAllMembers,
    selectRemoteMember,
    selectRemoteMembers,
    selectRemoteConvMembers,
    selectRemoteMembersByDomain,
    selectLocalMembersByDomain,
    insertUserRemoteConv,
    insertBot,
    insertConv,
    insertTeamConv,
    deleteTeamConv,
    markConvDeleted,
    deleteConv,
    selectConv,
    addMLSClient,
    selectGroupInfo,
    isConvDeleted,
    updateConvType,
    updateConvName,
    updateConvAccess,
    updateConvReceiptMode,
    updateConvMessageTimer,
    getConvEpoch,
    updateConvEpoch,
    updateConvCipherSuite,
    updateConvCellsState,
    resetConversation,
    updateGroupInfo,
    planMLSClientRemoval,
    ConvRow,
    selectUserConvsIn,
    selectUserConvsFrom,
    selectUserConvs,
    selectUserRemoteConvs,
    selectRemoteConvMemberStatuses,
    updateToMixedConv,
    updateToMLSConv,
    updateChannelAddPermission,
    acquireCommitLock,
    releaseCommitLock,
    updateOtrMemberMutedStatus,
    updateOtrMemberArchived,
    updateMemberHidden,
    updateRemoteOtrMemberMutedStatus,
    updateRemoteOtrMemberArchived,
    updateRemoteMemberHidden,
    updateMemberConvRoleName,
    updateRemoteMemberConvRoleName,
    lookupMLSClients,
    MemberStatus,
  )
where

import Cassandra as C hiding (Value)
import Cassandra.Util (Writetime)
import Data.Domain (Domain)
import Data.Id
import Data.Misc
import Imports
import Wire.API.Conversation
import Wire.API.Conversation.CellsState
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.GroupInfo

{-
TABLES:

conversation
member
member_remote_user
mls_commit_locks
mls_group_member_client
team_conv
user
user_remote_conv

-}

-- Teams --------------------------------------------------------------------

insertTeamConv :: PrepQuery W (TeamId, ConvId) ()
insertTeamConv = "insert into team_conv (team, conv) values (?, ?)"

deleteTeamConv :: PrepQuery W (TeamId, ConvId) ()
deleteTeamConv = "delete from team_conv where team = ? and conv = ?"

-- Conversations ------------------------------------------------------------

type ConvRow =
  ( ConvType,
    Maybe UserId,
    Maybe (C.Set Access),
    Maybe AccessRoleLegacy,
    Maybe (C.Set AccessRole),
    Maybe Text,
    Maybe TeamId,
    Maybe Bool,
    Maybe Milliseconds,
    Maybe ReceiptMode,
    Maybe ProtocolTag,
    Maybe GroupId,
    Maybe Epoch,
    Maybe (Writetime Epoch),
    Maybe CipherSuiteTag,
    Maybe GroupConvType,
    Maybe AddPermission,
    Maybe CellsState
  )

selectConv :: PrepQuery R (Identity ConvId) ConvRow
selectConv = "select type, creator, access, access_role, access_roles_v2, name, team, deleted, message_timer, receipt_mode, protocol, group_id, epoch, WRITETIME(epoch), cipher_suite, group_conv_type, channel_add_permission, cells_state  from conversation where conv = ?"

isConvDeleted :: PrepQuery R (Identity ConvId) (Identity (Maybe Bool))
isConvDeleted = "select deleted from conversation where conv = ?"

insertConv :: PrepQuery W (ConvId, ConvType, Maybe UserId, C.Set Access, C.Set AccessRole, Maybe Text, Maybe TeamId, Maybe Milliseconds, Maybe ReceiptMode, ProtocolTag, Maybe GroupId, Maybe GroupConvType, Maybe AddPermission, CellsState) ()
insertConv = "insert into conversation (conv, type, creator, access, access_roles_v2, name, team, message_timer, receipt_mode, protocol, group_id, group_conv_type, channel_add_permission, cells_state) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

insertMLSSelfConv ::
  PrepQuery
    W
    ( ConvId,
      ConvType,
      Maybe UserId,
      C.Set Access,
      C.Set AccessRole,
      Maybe Text,
      Maybe TeamId,
      Maybe Milliseconds,
      Maybe ReceiptMode,
      Maybe GroupId
    )
    ()
insertMLSSelfConv =
  fromString $
    "insert into conversation (conv, type, creator, access, \
    \ access_roles_v2, name, team, message_timer, receipt_mode,\
    \ protocol, group_id) values \
    \ (?, ?, ?, ?, ?, ?, ?, ?, ?, "
      <> show (fromEnum ProtocolMLSTag)
      <> ", ?)"

updateToMixedConv :: PrepQuery W (ConvId, ProtocolTag, GroupId, Epoch) ()
updateToMixedConv =
  "insert into conversation (conv, protocol, group_id, epoch) values (?, ?, ?, ?)"

updateToMLSConv :: PrepQuery W (ConvId, ProtocolTag) ()
updateToMLSConv = "insert into conversation (conv, protocol, receipt_mode) values (?, ?, 0)"

updateConvAccess :: PrepQuery W (C.Set Access, C.Set AccessRole, ConvId) ()
updateConvAccess = {- `IF EXISTS`, but that requires benchmarking -} "update conversation set access = ?, access_roles_v2 = ? where conv = ?"

updateConvReceiptMode :: PrepQuery W (ReceiptMode, ConvId) ()
updateConvReceiptMode = {- `IF EXISTS`, but that requires benchmarking -} "update conversation set receipt_mode = ? where conv = ?"

updateConvMessageTimer :: PrepQuery W (Maybe Milliseconds, ConvId) ()
updateConvMessageTimer = {- `IF EXISTS`, but that requires benchmarking -} "update conversation set message_timer = ? where conv = ?"

updateConvName :: PrepQuery W (Text, ConvId) ()
updateConvName = {- `IF EXISTS`, but that requires benchmarking -} "update conversation set name = ? where conv = ?"

updateConvType :: PrepQuery W (ConvType, ConvId) ()
updateConvType = {- `IF EXISTS`, but that requires benchmarking -} "update conversation set type = ? where conv = ?"

getConvEpoch :: PrepQuery R (Identity ConvId) (Identity (Maybe Epoch))
getConvEpoch = "select epoch from conversation where conv = ?"

updateConvEpoch :: PrepQuery W (Epoch, ConvId) ()
updateConvEpoch = {- `IF EXISTS`, but that requires benchmarking -} "update conversation set epoch = ? where conv = ?"

updateConvCipherSuite :: PrepQuery W (CipherSuiteTag, ConvId) ()
updateConvCipherSuite = "update conversation set cipher_suite = ? where conv = ?"

updateConvCellsState :: PrepQuery W (CellsState, ConvId) ()
updateConvCellsState = "update conversation set cells_state = ? where conv = ?"

resetConversation :: PrepQuery W (GroupId, ConvId) ()
resetConversation = "update conversation set group_id = ?, epoch = 0 where conv = ?"

deleteConv :: PrepQuery W (Identity ConvId) ()
deleteConv = "delete from conversation using timestamp 32503680000000000 where conv = ?"

markConvDeleted :: PrepQuery W (Identity ConvId) ()
markConvDeleted = {- `IF EXISTS`, but that requires benchmarking -} "update conversation set deleted = true where conv = ?"

selectGroupInfo :: PrepQuery R (Identity ConvId) (Identity (Maybe GroupInfoData))
selectGroupInfo = "select public_group_state from conversation where conv = ?"

updateGroupInfo :: PrepQuery W (GroupInfoData, ConvId) ()
updateGroupInfo = "update conversation set public_group_state = ? where conv = ?"

updateChannelAddPermission :: PrepQuery W (AddPermission, ConvId) ()
updateChannelAddPermission = "update conversation set channel_add_permission = ? where conv = ?"

-- User Conversations -------------------------------------------------------

selectUserConvsIn :: PrepQuery R (UserId, [ConvId]) (Identity ConvId)
selectUserConvsIn = "select conv from user where user = ? and conv in ? order by conv"

insertUserConv :: PrepQuery W (UserId, ConvId) ()
insertUserConv = "insert into user (user, conv) values (?, ?)"

deleteUserConv :: PrepQuery W (UserId, ConvId) ()
deleteUserConv = "delete from user where user = ? and conv = ?"

selectUserConvs :: PrepQuery R (Identity UserId) (Identity ConvId)
selectUserConvs = "select conv from user where user = ? order by conv"

selectUserConvsFrom :: PrepQuery R (UserId, ConvId) (Identity ConvId)
selectUserConvsFrom = "select conv from user where user = ? and conv > ? order by conv"

-- Members ------------------------------------------------------------------

type MemberStatus = Int32

selectMember :: PrepQuery R (ConvId, UserId) (UserId, Maybe ServiceId, Maybe ProviderId, Maybe MemberStatus, Maybe MutedStatus, Maybe Text, Maybe Bool, Maybe Text, Maybe Bool, Maybe Text, Maybe RoleName)
selectMember = "select user, service, provider, status, otr_muted_status, otr_muted_ref, otr_archived, otr_archived_ref, hidden, hidden_ref, conversation_role from member where conv = ? and user = ?"

selectMembers :: PrepQuery R (Identity ConvId) (UserId, Maybe ServiceId, Maybe ProviderId, Maybe MemberStatus, Maybe MutedStatus, Maybe Text, Maybe Bool, Maybe Text, Maybe Bool, Maybe Text, Maybe RoleName)
selectMembers = "select user, service, provider, status, otr_muted_status, otr_muted_ref, otr_archived, otr_archived_ref, hidden, hidden_ref, conversation_role from member where conv = ?"

selectAllMembers :: PrepQuery R () (UserId, Maybe ServiceId, Maybe ProviderId, Maybe MemberStatus, Maybe MutedStatus, Maybe Text, Maybe Bool, Maybe Text, Maybe Bool, Maybe Text, Maybe RoleName)
selectAllMembers = "select user, service, provider, status, otr_muted_status, otr_muted_ref, otr_archived, otr_archived_ref, hidden, hidden_ref, conversation_role from member"

insertMember :: PrepQuery W (ConvId, UserId, Maybe ServiceId, Maybe ProviderId, RoleName) ()
insertMember = "insert into member (conv, user, service, provider, status, conversation_role) values (?, ?, ?, ?, 0, ?)"

removeMember :: PrepQuery W (ConvId, UserId) ()
removeMember = "delete from member where conv = ? and user = ?"

updateOtrMemberMutedStatus :: PrepQuery W (MutedStatus, Maybe Text, ConvId, UserId) ()
updateOtrMemberMutedStatus = {- `IF EXISTS`, but that requires benchmarking -} "update member set otr_muted_status = ?, otr_muted_ref = ? where conv = ? and user = ?"

updateOtrMemberArchived :: PrepQuery W (Bool, Maybe Text, ConvId, UserId) ()
updateOtrMemberArchived = {- `IF EXISTS`, but that requires benchmarking -} "update member set otr_archived = ?, otr_archived_ref = ? where conv = ? and user = ?"

updateMemberHidden :: PrepQuery W (Bool, Maybe Text, ConvId, UserId) ()
updateMemberHidden = {- `IF EXISTS`, but that requires benchmarking -} "update member set hidden = ?, hidden_ref = ? where conv = ? and user = ?"

updateMemberConvRoleName :: PrepQuery W (RoleName, ConvId, UserId) ()
updateMemberConvRoleName = {- `IF EXISTS`, but that requires benchmarking -} "update member set conversation_role = ? where conv = ? and user = ?"

-- Federated conversations -----------------------------------------------------
--
-- FUTUREWORK(federation): allow queries for pagination to support more than 500 (?) conversations for a user.

-- local conversation with remote members

selectUserRemoteConvs :: PrepQuery R (Identity UserId) (Domain, ConvId)
selectUserRemoteConvs = "select conv_remote_domain, conv_remote_id from user_remote_conv where user = ?"

insertRemoteMember :: PrepQuery W (ConvId, Domain, UserId, RoleName) ()
insertRemoteMember = "insert into member_remote_user (conv, user_remote_domain, user_remote_id, conversation_role) values (?, ?, ?, ?)"

removeRemoteMember :: PrepQuery W (ConvId, Domain, UserId) ()
removeRemoteMember = "delete from member_remote_user where conv = ? and user_remote_domain = ? and user_remote_id = ?"

selectRemoteMember :: PrepQuery R (ConvId, Domain, UserId) (Identity RoleName)
selectRemoteMember = "select conversation_role from member_remote_user where conv = ? and user_remote_domain = ? and user_remote_id = ?"

selectRemoteMembers :: PrepQuery R (Identity ConvId) (Domain, UserId, RoleName)
selectRemoteMembers = "select user_remote_domain, user_remote_id, conversation_role from member_remote_user where conv = ?"

updateRemoteMemberConvRoleName :: PrepQuery W (RoleName, ConvId, Domain, UserId) ()
updateRemoteMemberConvRoleName = {- `IF EXISTS`, but that requires benchmarking -} "update member_remote_user set conversation_role = ? where conv = ? and user_remote_domain = ? and user_remote_id = ?"

-- Used when removing a federation domain, so that we can quickly list all of the affected remote users and conversations
-- This returns local conversation IDs and remote users
selectRemoteMembersByDomain :: PrepQuery R (Identity Domain) (ConvId, UserId, RoleName)
selectRemoteMembersByDomain = "select conv, user_remote_id, conversation_role from member_remote_user where user_remote_domain = ?"

-- local user with remote conversations

insertUserRemoteConv :: PrepQuery W (UserId, Domain, ConvId) ()
insertUserRemoteConv = "insert into user_remote_conv (user, conv_remote_domain, conv_remote_id) values (?, ?, ?)"

selectRemoteConvMemberStatuses :: PrepQuery R (UserId, Domain, [ConvId]) (ConvId, Maybe MutedStatus, Maybe Text, Maybe Bool, Maybe Text, Maybe Bool, Maybe Text)
selectRemoteConvMemberStatuses = "select conv_remote_id, otr_muted_status, otr_muted_ref, otr_archived, otr_archived_ref, hidden, hidden_ref from user_remote_conv where user = ? and conv_remote_domain = ? and conv_remote_id in ?"

selectRemoteConvMembers :: PrepQuery R (UserId, Domain, ConvId) (Identity UserId)
selectRemoteConvMembers = "select user from user_remote_conv where user = ? and conv_remote_domain = ? and conv_remote_id = ?"

deleteUserRemoteConv :: PrepQuery W (UserId, Domain, ConvId) ()
deleteUserRemoteConv = "delete from user_remote_conv where user = ? and conv_remote_domain = ? and conv_remote_id = ?"

-- Used when removing a federation domain, so that we can quickly list all of the affected local users and conversations
-- This returns remote conversation IDs and local users
selectLocalMembersByDomain :: PrepQuery R (Identity Domain) (ConvId, UserId)
selectLocalMembersByDomain = "select conv_remote_id, user from user_remote_conv where conv_remote_domain = ?"

-- remote conversation status for local user

updateRemoteOtrMemberMutedStatus :: PrepQuery W (MutedStatus, Maybe Text, Domain, ConvId, UserId) ()
updateRemoteOtrMemberMutedStatus = {- `IF EXISTS`, but that requires benchmarking -} "update user_remote_conv set otr_muted_status = ?, otr_muted_ref = ? where conv_remote_domain = ? and conv_remote_id = ? and user = ?"

updateRemoteOtrMemberArchived :: PrepQuery W (Bool, Maybe Text, Domain, ConvId, UserId) ()
updateRemoteOtrMemberArchived = {- `IF EXISTS`, but that requires benchmarking -} "update user_remote_conv set otr_archived = ?, otr_archived_ref = ? where conv_remote_domain = ? and conv_remote_id = ? and user = ?"

updateRemoteMemberHidden :: PrepQuery W (Bool, Maybe Text, Domain, ConvId, UserId) ()
updateRemoteMemberHidden = {- `IF EXISTS`, but that requires benchmarking -} "update user_remote_conv set hidden = ?, hidden_ref = ? where conv_remote_domain = ? and conv_remote_id = ? and user = ?"

-- MLS Clients --------------------------------------------------------------

addMLSClient :: PrepQuery W (GroupId, Domain, UserId, ClientId, Int32) ()
addMLSClient = "insert into mls_group_member_client (group_id, user_domain, user, client, leaf_node_index, removal_pending) values (?, ?, ?, ?, ?, false)"

planMLSClientRemoval :: PrepQuery W (GroupId, Domain, UserId, ClientId) ()
planMLSClientRemoval = "update mls_group_member_client set removal_pending = true where group_id = ? and user_domain = ? and user = ? and client = ?"

removeMLSClient :: PrepQuery W (GroupId, Domain, UserId, ClientId) ()
removeMLSClient = "delete from mls_group_member_client where group_id = ? and user_domain = ? and user = ? and client = ?"

removeAllMLSClients :: PrepQuery W (Identity GroupId) ()
removeAllMLSClients = "DELETE FROM mls_group_member_client WHERE group_id = ?"

lookupMLSClients :: PrepQuery R (Identity GroupId) (Domain, UserId, ClientId, Int32, Bool)
lookupMLSClients = "select user_domain, user, client, leaf_node_index, removal_pending from mls_group_member_client where group_id = ?"

acquireCommitLock :: PrepQuery W (GroupId, Epoch, Int32) Row
acquireCommitLock = "insert into mls_commit_locks (group_id, epoch) values (?, ?) if not exists using ttl ?"

releaseCommitLock :: PrepQuery W (GroupId, Epoch) ()
releaseCommitLock = "delete from mls_commit_locks where group_id = ? and epoch = ?"

-- Bots ---------------------------------------------------------------------

insertBot :: PrepQuery W (ConvId, BotId, ServiceId, ProviderId) ()
insertBot = "insert into member (conv, user, service, provider, status) values (?, ?, ?, ?, 0)"

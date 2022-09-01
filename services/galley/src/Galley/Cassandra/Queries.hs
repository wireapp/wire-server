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

module Galley.Cassandra.Queries where

import Cassandra as C hiding (Value)
import Cassandra.Util (Writetime)
import Data.Domain (Domain)
import Data.Id
import Data.Json.Util
import Data.LegalHold
import Data.Misc
import qualified Data.Text.Lazy as LT
import Galley.Data.Scope
import Galley.Types.Teams.Intra
import Imports
import Text.RawString.QQ
import Wire.API.Conversation
import Wire.API.Conversation.Code
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.KeyPackage
import Wire.API.Provider
import Wire.API.Provider.Service
import Wire.API.Team
import Wire.API.Team.Permission
import Wire.API.Team.SearchVisibility
import Wire.API.User.Client.Prekey

-- Teams --------------------------------------------------------------------

selectTeam :: PrepQuery R (Identity TeamId) (UserId, Text, Icon, Maybe Text, Bool, Maybe TeamStatus, Maybe (Writetime TeamStatus), Maybe TeamBinding, Maybe Icon)
selectTeam = "select creator, name, icon, icon_key, deleted, status, writetime(status), binding, splash_screen from team where team = ?"

selectTeamName :: PrepQuery R (Identity TeamId) (Identity Text)
selectTeamName = "select name from team where team = ?"

selectTeamBinding :: PrepQuery R (Identity TeamId) (Identity (Maybe TeamBinding))
selectTeamBinding = "select binding from team where team = ?"

selectTeamBindingWritetime :: PrepQuery R (Identity TeamId) (Identity (Maybe Int64))
selectTeamBindingWritetime = "select writetime(binding) from team where team = ?"

selectTeamConv :: PrepQuery R (TeamId, ConvId) (Identity ConvId)
selectTeamConv = "select conv from team_conv where team = ? and conv = ?"

selectTeamConvs :: PrepQuery R (Identity TeamId) (Identity ConvId)
selectTeamConvs = "select conv from team_conv where team = ? order by conv"

selectTeamConvsFrom :: PrepQuery R (TeamId, ConvId) (Identity ConvId)
selectTeamConvsFrom = "select conv from team_conv where team = ? and conv > ? order by conv"

selectTeamMember ::
  PrepQuery
    R
    (TeamId, UserId)
    ( Permissions,
      Maybe UserId,
      Maybe UTCTimeMillis,
      Maybe UserLegalHoldStatus
    )
selectTeamMember = "select perms, invited_by, invited_at, legalhold_status from team_member where team = ? and user = ?"

-- | This query fetches **all** members of a team, it should always be paginated
selectTeamMembers ::
  PrepQuery
    R
    (Identity TeamId)
    ( UserId,
      Permissions,
      Maybe UserId,
      Maybe UTCTimeMillis,
      Maybe UserLegalHoldStatus
    )
selectTeamMembers =
  [r|
    select user, perms, invited_by, invited_at, legalhold_status
      from team_member
    where team = ? order by user
    |]

selectTeamMembersFrom ::
  PrepQuery
    R
    (TeamId, UserId)
    ( UserId,
      Permissions,
      Maybe UserId,
      Maybe UTCTimeMillis,
      Maybe UserLegalHoldStatus
    )
selectTeamMembersFrom =
  [r|
    select user, perms, invited_by, invited_at, legalhold_status
      from team_member
    where team = ? and user > ? order by user
    |]

selectTeamMembers' ::
  PrepQuery
    R
    (TeamId, [UserId])
    ( UserId,
      Permissions,
      Maybe UserId,
      Maybe UTCTimeMillis,
      Maybe UserLegalHoldStatus
    )
selectTeamMembers' =
  [r|
    select user, perms, invited_by, invited_at, legalhold_status
      from team_member
    where team = ? and user in ? order by user
    |]

selectUserTeams :: PrepQuery R (Identity UserId) (Identity TeamId)
selectUserTeams = "select team from user_team where user = ? order by team"

selectOneUserTeam :: PrepQuery R (Identity UserId) (Identity TeamId)
selectOneUserTeam = "select team from user_team where user = ? limit 1"

selectUserTeamsIn :: PrepQuery R (UserId, [TeamId]) (Identity TeamId)
selectUserTeamsIn = "select team from user_team where user = ? and team in ? order by team"

selectUserTeamsFrom :: PrepQuery R (UserId, TeamId) (Identity TeamId)
selectUserTeamsFrom = "select team from user_team where user = ? and team > ? order by team"

insertTeam :: PrepQuery W (TeamId, UserId, Text, Icon, Maybe Text, TeamStatus, TeamBinding) ()
insertTeam = "insert into team (team, creator, name, icon, icon_key, deleted, status, binding) values (?, ?, ?, ?, ?, false, ?, ?)"

insertTeamConv :: PrepQuery W (TeamId, ConvId) ()
insertTeamConv = "insert into team_conv (team, conv) values (?, ?)"

deleteTeamConv :: PrepQuery W (TeamId, ConvId) ()
deleteTeamConv = "delete from team_conv where team = ? and conv = ?"

insertTeamMember :: PrepQuery W (TeamId, UserId, Permissions, Maybe UserId, Maybe UTCTimeMillis) ()
insertTeamMember = "insert into team_member (team, user, perms, invited_by, invited_at) values (?, ?, ?, ?, ?)"

deleteTeamMember :: PrepQuery W (TeamId, UserId) ()
deleteTeamMember = "delete from team_member where team = ? and user = ?"

insertBillingTeamMember :: PrepQuery W (TeamId, UserId) ()
insertBillingTeamMember = "insert into billing_team_member (team, user) values (?, ?)"

deleteBillingTeamMember :: PrepQuery W (TeamId, UserId) ()
deleteBillingTeamMember = "delete from billing_team_member where team = ? and user = ?"

listBillingTeamMembers :: PrepQuery R (Identity TeamId) (Identity UserId)
listBillingTeamMembers = "select user from billing_team_member where team = ?"

updatePermissions :: PrepQuery W (Permissions, TeamId, UserId) ()
updatePermissions = "update team_member set perms = ? where team = ? and user = ?"

insertUserTeam :: PrepQuery W (UserId, TeamId) ()
insertUserTeam = "insert into user_team (user, team) values (?, ?)"

deleteUserTeam :: PrepQuery W (UserId, TeamId) ()
deleteUserTeam = "delete from user_team where user = ? and team = ?"

markTeamDeleted :: PrepQuery W (TeamStatus, TeamId) ()
markTeamDeleted = "update team set status = ? where team = ?"

deleteTeam :: PrepQuery W (TeamStatus, TeamId) ()
deleteTeam = "update team using timestamp 32503680000000000 set name = 'default', icon = 'default', status = ? where team = ? "

updateTeamName :: PrepQuery W (Text, TeamId) ()
updateTeamName = "update team set name = ? where team = ?"

updateTeamIcon :: PrepQuery W (Text, TeamId) ()
updateTeamIcon = "update team set icon = ? where team = ?"

updateTeamIconKey :: PrepQuery W (Text, TeamId) ()
updateTeamIconKey = "update team set icon_key = ? where team = ?"

updateTeamStatus :: PrepQuery W (TeamStatus, TeamId) ()
updateTeamStatus = "update team set status = ? where team = ?"

updateTeamSplashScreen :: PrepQuery W (Text, TeamId) ()
updateTeamSplashScreen = "update team set splash_screen = ? where team = ?"

-- Conversations ------------------------------------------------------------

selectConv :: PrepQuery R (Identity ConvId) (ConvType, UserId, Maybe (C.Set Access), Maybe AccessRoleLegacy, Maybe (C.Set AccessRoleV2), Maybe Text, Maybe TeamId, Maybe Bool, Maybe Milliseconds, Maybe ReceiptMode, Maybe ProtocolTag, Maybe GroupId, Maybe Epoch, Maybe CipherSuiteTag)
selectConv = "select type, creator, access, access_role, access_roles_v2, name, team, deleted, message_timer, receipt_mode, protocol, group_id, epoch, cipher_suite from conversation where conv = ?"

selectReceiptMode :: PrepQuery R (Identity ConvId) (Identity (Maybe ReceiptMode))
selectReceiptMode = "select receipt_mode from conversation where conv = ?"

isConvDeleted :: PrepQuery R (Identity ConvId) (Identity (Maybe Bool))
isConvDeleted = "select deleted from conversation where conv = ?"

insertConv :: PrepQuery W (ConvId, ConvType, UserId, C.Set Access, C.Set AccessRoleV2, Maybe Text, Maybe TeamId, Maybe Milliseconds, Maybe ReceiptMode, ProtocolTag, Maybe GroupId, Maybe Epoch, Maybe CipherSuiteTag) ()
insertConv = "insert into conversation (conv, type, creator, access, access_roles_v2, name, team, message_timer, receipt_mode, protocol, group_id, epoch, cipher_suite) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

updateConvAccess :: PrepQuery W (C.Set Access, C.Set AccessRoleV2, ConvId) ()
updateConvAccess = "update conversation set access = ?, access_roles_v2 = ? where conv = ?"

updateConvReceiptMode :: PrepQuery W (ReceiptMode, ConvId) ()
updateConvReceiptMode = "update conversation set receipt_mode = ? where conv = ?"

updateConvMessageTimer :: PrepQuery W (Maybe Milliseconds, ConvId) ()
updateConvMessageTimer = "update conversation set message_timer = ? where conv = ?"

updateConvName :: PrepQuery W (Text, ConvId) ()
updateConvName = "update conversation set name = ? where conv = ?"

updateConvType :: PrepQuery W (ConvType, ConvId) ()
updateConvType = "update conversation set type = ? where conv = ?"

updateConvEpoch :: PrepQuery W (Epoch, ConvId) ()
updateConvEpoch = "update conversation set epoch = ? where conv = ?"

deleteConv :: PrepQuery W (Identity ConvId) ()
deleteConv = "delete from conversation using timestamp 32503680000000000 where conv = ?"

markConvDeleted :: PrepQuery W (Identity ConvId) ()
markConvDeleted = "update conversation set deleted = true where conv = ?"

-- Conversations accessible by code -----------------------------------------

insertCode :: PrepQuery W (Key, Value, ConvId, Scope, Int32) ()
insertCode = "INSERT INTO conversation_codes (key, value, conversation, scope) VALUES (?, ?, ?, ?) USING TTL ?"

lookupCode :: PrepQuery R (Key, Scope) (Value, Int32, ConvId)
lookupCode = "SELECT value, ttl(value), conversation FROM conversation_codes WHERE key = ? AND scope = ?"

deleteCode :: PrepQuery W (Key, Scope) ()
deleteCode = "DELETE FROM conversation_codes WHERE key = ? AND scope = ?"

-- User Conversations -------------------------------------------------------

selectUserConvs :: PrepQuery R (Identity UserId) (Identity ConvId)
selectUserConvs = "select conv from user where user = ? order by conv"

selectUserConvsIn :: PrepQuery R (UserId, [ConvId]) (Identity ConvId)
selectUserConvsIn = "select conv from user where user = ? and conv in ? order by conv"

selectUserConvsFrom :: PrepQuery R (UserId, ConvId) (Identity ConvId)
selectUserConvsFrom = "select conv from user where user = ? and conv > ? order by conv"

insertUserConv :: PrepQuery W (UserId, ConvId) ()
insertUserConv = "insert into user (user, conv) values (?, ?)"

deleteUserConv :: PrepQuery W (UserId, ConvId) ()
deleteUserConv = "delete from user where user = ? and conv = ?"

-- MLS Conversations --------------------------------------------------------

insertGroupId :: PrepQuery W (GroupId, ConvId, Domain) ()
insertGroupId = "INSERT INTO group_id_conv_id (group_id, conv_id, domain) VALUES (?, ?, ?)"

lookupGroupId :: PrepQuery R (Identity GroupId) (ConvId, Domain)
lookupGroupId = "SELECT conv_id, domain from group_id_conv_id where group_id = ?"

-- Members ------------------------------------------------------------------

type MemberStatus = Int32

selectMember :: PrepQuery R (ConvId, UserId) (UserId, Maybe ServiceId, Maybe ProviderId, Maybe MemberStatus, Maybe MutedStatus, Maybe Text, Maybe Bool, Maybe Text, Maybe Bool, Maybe Text, Maybe RoleName, Maybe (C.Set (ClientId, KeyPackageRef)))
selectMember = "select user, service, provider, status, otr_muted_status, otr_muted_ref, otr_archived, otr_archived_ref, hidden, hidden_ref, conversation_role, mls_clients_keypackages from member where conv = ? and user = ?"

selectMembers :: PrepQuery R (Identity ConvId) (UserId, Maybe ServiceId, Maybe ProviderId, Maybe MemberStatus, Maybe MutedStatus, Maybe Text, Maybe Bool, Maybe Text, Maybe Bool, Maybe Text, Maybe RoleName, Maybe (C.Set (ClientId, KeyPackageRef)))
selectMembers = "select user, service, provider, status, otr_muted_status, otr_muted_ref, otr_archived, otr_archived_ref, hidden, hidden_ref, conversation_role, mls_clients_keypackages from member where conv = ?"

insertMember :: PrepQuery W (ConvId, UserId, Maybe ServiceId, Maybe ProviderId, RoleName, Maybe (C.Set (ClientId, KeyPackageRef))) ()
insertMember = "insert into member (conv, user, service, provider, status, conversation_role, mls_clients_keypackages) values (?, ?, ?, ?, 0, ?, ?)"

removeMember :: PrepQuery W (ConvId, UserId) ()
removeMember = "delete from member where conv = ? and user = ?"

updateOtrMemberMutedStatus :: PrepQuery W (MutedStatus, Maybe Text, ConvId, UserId) ()
updateOtrMemberMutedStatus = "update member set otr_muted_status = ?, otr_muted_ref = ? where conv = ? and user = ?"

updateOtrMemberArchived :: PrepQuery W (Bool, Maybe Text, ConvId, UserId) ()
updateOtrMemberArchived = "update member set otr_archived = ?, otr_archived_ref = ? where conv = ? and user = ?"

updateMemberHidden :: PrepQuery W (Bool, Maybe Text, ConvId, UserId) ()
updateMemberHidden = "update member set hidden = ?, hidden_ref = ? where conv = ? and user = ?"

updateMemberConvRoleName :: PrepQuery W (RoleName, ConvId, UserId) ()
updateMemberConvRoleName = "update member set conversation_role = ? where conv = ? and user = ?"

-- Federated conversations -----------------------------------------------------
--
-- FUTUREWORK(federation): allow queries for pagination to support more than 500 (?) conversations for a user.

-- local conversation with remote members

insertRemoteMember :: PrepQuery W (ConvId, Domain, UserId, RoleName) ()
insertRemoteMember = "insert into member_remote_user (conv, user_remote_domain, user_remote_id, conversation_role) values (?, ?, ?, ?)"

removeRemoteMember :: PrepQuery W (ConvId, Domain, UserId) ()
removeRemoteMember = "delete from member_remote_user where conv = ? and user_remote_domain = ? and user_remote_id = ?"

selectRemoteMember :: PrepQuery R (ConvId, Domain, UserId) (RoleName, C.Set (ClientId, KeyPackageRef))
selectRemoteMember = "select conversation_role, mls_clients_keypackages from member_remote_user where conv = ? and user_remote_domain = ? and user_remote_id = ?"

selectRemoteMembers :: PrepQuery R (Identity ConvId) (Domain, UserId, RoleName, C.Set (ClientId, KeyPackageRef))
selectRemoteMembers = "select user_remote_domain, user_remote_id, conversation_role, mls_clients_keypackages from member_remote_user where conv = ?"

updateRemoteMemberConvRoleName :: PrepQuery W (RoleName, ConvId, Domain, UserId) ()
updateRemoteMemberConvRoleName = "update member_remote_user set conversation_role = ? where conv = ? and user_remote_domain = ? and user_remote_id = ?"

-- local user with remote conversations

insertUserRemoteConv :: PrepQuery W (UserId, Domain, ConvId) ()
insertUserRemoteConv = "insert into user_remote_conv (user, conv_remote_domain, conv_remote_id) values (?, ?, ?)"

selectUserRemoteConvs :: PrepQuery R (Identity UserId) (Domain, ConvId)
selectUserRemoteConvs = "select conv_remote_domain, conv_remote_id from user_remote_conv where user = ?"

selectRemoteConvMemberStatuses :: PrepQuery R (UserId, Domain, [ConvId]) (ConvId, Maybe MutedStatus, Maybe Text, Maybe Bool, Maybe Text, Maybe Bool, Maybe Text)
selectRemoteConvMemberStatuses = "select conv_remote_id, otr_muted_status, otr_muted_ref, otr_archived, otr_archived_ref, hidden, hidden_ref from user_remote_conv where user = ? and conv_remote_domain = ? and conv_remote_id in ?"

selectRemoteConvMembers :: PrepQuery R (UserId, Domain, ConvId) (Identity UserId)
selectRemoteConvMembers = "select user from user_remote_conv where user = ? and conv_remote_domain = ? and conv_remote_id = ?"

deleteUserRemoteConv :: PrepQuery W (UserId, Domain, ConvId) ()
deleteUserRemoteConv = "delete from user_remote_conv where user = ? and conv_remote_domain = ? and conv_remote_id = ?"

-- remote conversation status for local user

updateRemoteOtrMemberMutedStatus :: PrepQuery W (MutedStatus, Maybe Text, Domain, ConvId, UserId) ()
updateRemoteOtrMemberMutedStatus = "update user_remote_conv set otr_muted_status = ?, otr_muted_ref = ? where conv_remote_domain = ? and conv_remote_id = ? and user = ?"

updateRemoteOtrMemberArchived :: PrepQuery W (Bool, Maybe Text, Domain, ConvId, UserId) ()
updateRemoteOtrMemberArchived = "update user_remote_conv set otr_archived = ?, otr_archived_ref = ? where conv_remote_domain = ? and conv_remote_id = ? and user = ?"

updateRemoteMemberHidden :: PrepQuery W (Bool, Maybe Text, Domain, ConvId, UserId) ()
updateRemoteMemberHidden = "update user_remote_conv set hidden = ?, hidden_ref = ? where conv_remote_domain = ? and conv_remote_id = ? and user = ?"

selectRemoteMemberStatus :: PrepQuery R (Domain, ConvId, UserId) (Maybe MutedStatus, Maybe Text, Maybe Bool, Maybe Text, Maybe Bool, Maybe Text)
selectRemoteMemberStatus = "select otr_muted_status, otr_muted_ref, otr_archived, otr_archived_ref, hidden, hidden_ref from user_remote_conv where conv_remote_domain = ? and conv_remote_id = ? and user = ?"

-- Clients ------------------------------------------------------------------

selectClients :: PrepQuery R (Identity [UserId]) (UserId, C.Set ClientId)
selectClients = "select user, clients from clients where user in ?"

rmClients :: PrepQuery W (Identity UserId) ()
rmClients = "delete from clients where user = ?"

addMemberClient :: ClientId -> QueryString W (Identity UserId) ()
addMemberClient c =
  let t = LT.fromStrict (client c)
   in QueryString $ "update clients set clients = clients + {'" <> t <> "'} where user = ?"

rmMemberClient :: ClientId -> QueryString W (Identity UserId) ()
rmMemberClient c =
  let t = LT.fromStrict (client c)
   in QueryString $ "update clients set clients = clients - {'" <> t <> "'} where user = ?"

-- MLS Clients --------------------------------------------------------------

addLocalMLSClients :: PrepQuery W (C.Set (ClientId, KeyPackageRef), ConvId, UserId) ()
addLocalMLSClients = "update member set mls_clients_keypackages = mls_clients_keypackages + ? where conv = ? and user = ?"

addRemoteMLSClients :: PrepQuery W (C.Set (ClientId, KeyPackageRef), ConvId, Domain, UserId) ()
addRemoteMLSClients = "update member_remote_user set mls_clients_keypackages = mls_clients_keypackages + ? where conv = ? and user_remote_domain = ? and user_remote_id = ?"

removeLocalMLSClients :: PrepQuery W (C.Set (ClientId, KeyPackageRef), ConvId, UserId) ()
removeLocalMLSClients = "update member set mls_clients_keypackages = mls_clients_keypackages - ? where conv = ? and user = ?"

removeRemoteMLSClients :: PrepQuery W (C.Set (ClientId, KeyPackageRef), ConvId, Domain, UserId) ()
removeRemoteMLSClients = "update member_remote_user set mls_clients_keypackages = mls_clients_keypackages - ? where conv = ? and user_remote_domain = ? and user_remote_id = ?"

acquireCommitLock :: PrepQuery W (GroupId, Epoch, Int32) Row
acquireCommitLock = "insert into mls_commit_locks (group_id, epoch) values (?, ?) if not exists using ttl ?"

releaseCommitLock :: PrepQuery W (GroupId, Epoch) ()
releaseCommitLock = "delete from mls_commit_locks where group_id = ? and epoch = ?"

-- Services -----------------------------------------------------------------

rmSrv :: PrepQuery W (ProviderId, ServiceId) ()
rmSrv = "delete from service where provider = ? AND id = ?"

insertSrv :: PrepQuery W (ProviderId, ServiceId, HttpsUrl, ServiceToken, C.Set (Fingerprint Rsa), Bool) ()
insertSrv = "insert into service (provider, id, base_url, auth_token, fingerprints, enabled) values (?, ?, ?, ?, ?, ?)"

selectSrv :: PrepQuery R (ProviderId, ServiceId) (HttpsUrl, ServiceToken, C.Set (Fingerprint Rsa), Bool)
selectSrv = "select base_url, auth_token, fingerprints, enabled from service where provider = ? AND id = ?"

-- Bots ---------------------------------------------------------------------

insertBot :: PrepQuery W (ConvId, BotId, ServiceId, ProviderId) ()
insertBot = "insert into member (conv, user, service, provider, status) values (?, ?, ?, ?, 0)"

-- LegalHold ----------------------------------------------------------------

insertLegalHoldSettings :: PrepQuery W (HttpsUrl, Fingerprint Rsa, ServiceToken, ServiceKey, TeamId) ()
insertLegalHoldSettings =
  [r|
    update legalhold_service
    set base_url    = ?,
        fingerprint = ?,
        auth_token  = ?,
        pubkey      = ?
    where team_id = ?
  |]

selectLegalHoldSettings :: PrepQuery R (Identity TeamId) (HttpsUrl, Fingerprint Rsa, ServiceToken, ServiceKey)
selectLegalHoldSettings =
  [r|
   select base_url, fingerprint, auth_token, pubkey
     from legalhold_service
     where team_id = ?
   |]

removeLegalHoldSettings :: PrepQuery W (Identity TeamId) ()
removeLegalHoldSettings = "delete from legalhold_service where team_id = ?"

insertPendingPrekeys :: PrepQuery W (UserId, PrekeyId, Text) ()
insertPendingPrekeys =
  [r|
        insert into legalhold_pending_prekeys (user, key, data) values (?, ?, ?)
    |]

dropPendingPrekeys :: PrepQuery W (Identity UserId) ()
dropPendingPrekeys =
  [r|
        delete from legalhold_pending_prekeys
          where user = ?
    |]

selectPendingPrekeys :: PrepQuery R (Identity UserId) (PrekeyId, Text)
selectPendingPrekeys =
  [r|
        select key, data
          from legalhold_pending_prekeys
          where user = ?
          order by key asc
    |]

updateUserLegalHoldStatus :: PrepQuery W (UserLegalHoldStatus, TeamId, UserId) ()
updateUserLegalHoldStatus =
  [r|
        update team_member
          set legalhold_status = ?
          where team = ? and user = ?
    |]

selectLegalHoldWhitelistedTeam :: PrepQuery R (Identity TeamId) (Identity TeamId)
selectLegalHoldWhitelistedTeam =
  [r|
        select team from legalhold_whitelisted where team = ?
    |]

insertLegalHoldWhitelistedTeam :: PrepQuery W (Identity TeamId) ()
insertLegalHoldWhitelistedTeam =
  [r|
        insert into legalhold_whitelisted (team) values (?)
    |]

removeLegalHoldWhitelistedTeam :: PrepQuery W (Identity TeamId) ()
removeLegalHoldWhitelistedTeam =
  [r|
        delete from legalhold_whitelisted where team = ?
    |]

-- Search Visibility --------------------------------------------------------

selectSearchVisibility :: PrepQuery R (Identity TeamId) (Identity (Maybe TeamSearchVisibility))
selectSearchVisibility =
  "select search_visibility from team where team = ?"

updateSearchVisibility :: PrepQuery W (TeamSearchVisibility, TeamId) ()
updateSearchVisibility =
  "update team set search_visibility = ? where team = ?"

-- Custom Backend -----------------------------------------------------------

selectCustomBackend :: PrepQuery R (Identity Domain) (HttpsUrl, HttpsUrl)
selectCustomBackend =
  "select config_json_url, webapp_welcome_url from custom_backend where domain = ?"

updateCustomBackend :: PrepQuery W (HttpsUrl, HttpsUrl, Domain) ()
updateCustomBackend =
  "update custom_backend set config_json_url = ?, webapp_welcome_url = ? where domain = ?"

deleteCustomBackend :: PrepQuery W (Identity Domain) ()
deleteCustomBackend =
  "delete from custom_backend where domain = ?"

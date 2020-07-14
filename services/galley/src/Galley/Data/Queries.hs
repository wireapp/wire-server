-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.Data.Queries where

import Brig.Types.Client.Prekey
import Brig.Types.Code
import Brig.Types.Provider
import Cassandra as C hiding (Value)
import Cassandra.Util (Writetime)
import Data.Domain (Domain)
import Data.Id
import Data.Json.Util
import Data.LegalHold
import Data.Misc
import qualified Data.Text.Lazy as LT
import Galley.Data.Types
import Galley.Types hiding (Conversation)
import Galley.Types.Conversations.Roles
import Galley.Types.Teams
import Galley.Types.Teams.Intra
import Galley.Types.Teams.SearchVisibility
import Imports
import Text.RawString.QQ

-- Teams --------------------------------------------------------------------

selectTeam :: PrepQuery R (Identity TeamId) (UserId, Text, Text, Maybe Text, Bool, Maybe TeamStatus, Maybe (Writetime TeamStatus), Maybe TeamBinding)
selectTeam = "select creator, name, icon, icon_key, deleted, status, writetime(status), binding from team where team = ?"

selectTeamName :: PrepQuery R (Identity TeamId) (Identity Text)
selectTeamName = "select name from team where team = ?"

selectTeamBinding :: PrepQuery R (Identity TeamId) (Identity (Maybe TeamBinding))
selectTeamBinding = "select binding from team where team = ?"

selectTeamBindingWritetime :: PrepQuery R (Identity TeamId) (Identity (Maybe Int64))
selectTeamBindingWritetime = "select writetime(binding) from team where team = ?"

selectTeamConv :: PrepQuery R (TeamId, ConvId) (Identity Bool)
selectTeamConv = "select managed from team_conv where team = ? and conv = ?"

selectTeamConvs :: PrepQuery R (Identity TeamId) (ConvId, Bool)
selectTeamConvs = "select conv, managed from team_conv where team = ? order by conv"

selectTeamConvsFrom :: PrepQuery R (TeamId, OpaqueConvId) (ConvId, Bool)
selectTeamConvsFrom = "select conv, managed from team_conv where team = ? and conv > ? order by conv"

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

insertTeam :: PrepQuery W (TeamId, UserId, Text, Text, Maybe Text, TeamStatus, TeamBinding) ()
insertTeam = "insert into team (team, creator, name, icon, icon_key, deleted, status, binding) values (?, ?, ?, ?, ?, false, ?, ?)"

insertTeamConv :: PrepQuery W (TeamId, ConvId, Bool) ()
insertTeamConv = "insert into team_conv (team, conv, managed) values (?, ?, ?)"

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

-- Conversations ------------------------------------------------------------

selectConv :: PrepQuery R (Identity ConvId) (ConvType, UserId, Maybe (C.Set Access), Maybe AccessRole, Maybe Text, Maybe TeamId, Maybe Bool, Maybe Milliseconds, Maybe ReceiptMode)
selectConv = "select type, creator, access, access_role, name, team, deleted, message_timer, receipt_mode from conversation where conv = ?"

selectConvs :: PrepQuery R (Identity [ConvId]) (ConvId, ConvType, UserId, Maybe (C.Set Access), Maybe AccessRole, Maybe Text, Maybe TeamId, Maybe Bool, Maybe Milliseconds, Maybe ReceiptMode)
selectConvs = "select conv, type, creator, access, access_role, name, team, deleted, message_timer, receipt_mode from conversation where conv in ?"

selectReceiptMode :: PrepQuery R (Identity ConvId) (Identity (Maybe ReceiptMode))
selectReceiptMode = "select receipt_mode from conversation where conv = ?"

isConvDeleted :: PrepQuery R (Identity ConvId) (Identity (Maybe Bool))
isConvDeleted = "select deleted from conversation where conv = ?"

insertConv :: PrepQuery W (ConvId, ConvType, UserId, C.Set Access, AccessRole, Maybe Text, Maybe TeamId, Maybe Milliseconds, Maybe ReceiptMode) ()
insertConv = "insert into conversation (conv, type, creator, access, access_role, name, team, message_timer, receipt_mode) values (?, ?, ?, ?, ?, ?, ?, ?, ?)"

updateConvAccess :: PrepQuery W (C.Set Access, AccessRole, ConvId) ()
updateConvAccess = "update conversation set access = ?, access_role = ? where conv = ?"

updateConvReceiptMode :: PrepQuery W (ReceiptMode, ConvId) ()
updateConvReceiptMode = "update conversation set receipt_mode = ? where conv = ?"

updateConvMessageTimer :: PrepQuery W (Maybe Milliseconds, ConvId) ()
updateConvMessageTimer = "update conversation set message_timer = ? where conv = ?"

updateConvName :: PrepQuery W (Text, ConvId) ()
updateConvName = "update conversation set name = ? where conv = ?"

updateConvType :: PrepQuery W (ConvType, ConvId) ()
updateConvType = "update conversation set type = ? where conv = ?"

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

selectUserConvs :: PrepQuery R (Identity UserId) (OpaqueConvId, Maybe RemoteConvId, Maybe Domain)
selectUserConvs = "select conv, conv_remote_id, conv_remote_domain from user where user = ? order by conv"

selectUserConvsIn :: PrepQuery R (UserId, [OpaqueConvId]) (OpaqueConvId, Maybe RemoteConvId, Maybe Domain)
selectUserConvsIn = "select conv, conv_remote_id, conv_remote_domain from user where user = ? and conv in ? order by conv"

selectUserConvsFrom :: PrepQuery R (UserId, OpaqueConvId) (OpaqueConvId, Maybe RemoteConvId, Maybe Domain)
selectUserConvsFrom = "select conv, conv_remote_id, conv_remote_domain from user where user = ? and conv > ? order by conv"

insertUserConv :: PrepQuery W (UserId, OpaqueConvId, Maybe RemoteConvId, Maybe Domain) ()
insertUserConv = "insert into user (user, conv, conv_remote_id, conv_remote_domain) values (?, ?, ?, ?)"

deleteUserConv :: PrepQuery W (UserId, OpaqueConvId) ()
deleteUserConv = "delete from user where user = ? and conv = ?"

-- Members ------------------------------------------------------------------

type MemberStatus = Int32

selectMember :: PrepQuery R (ConvId, OpaqueUserId) (OpaqueUserId, Maybe RemoteUserId, Maybe Domain, Maybe ServiceId, Maybe ProviderId, Maybe MemberStatus, Maybe Bool, Maybe MutedStatus, Maybe Text, Maybe Bool, Maybe Text, Maybe Bool, Maybe Text, Maybe RoleName)
selectMember = "select user, user_remote_id, user_remote_domain, service, provider, status, otr_muted, otr_muted_status, otr_muted_ref, otr_archived, otr_archived_ref, hidden, hidden_ref, conversation_role from member where conv = ? and user = ?"

selectMembers :: PrepQuery R (Identity [ConvId]) (ConvId, OpaqueUserId, Maybe RemoteUserId, Maybe Domain, Maybe ServiceId, Maybe ProviderId, Maybe MemberStatus, Maybe Bool, Maybe MutedStatus, Maybe Text, Maybe Bool, Maybe Text, Maybe Bool, Maybe Text, Maybe RoleName)
selectMembers = "select conv, user, user_remote_id, user_remote_domain, service, provider, status, otr_muted, otr_muted_status, otr_muted_ref, otr_archived, otr_archived_ref, hidden, hidden_ref, conversation_role from member where conv in ?"

insertMember :: PrepQuery W (ConvId, OpaqueUserId, Maybe RemoteUserId, Maybe Domain, Maybe ServiceId, Maybe ProviderId, RoleName) ()
insertMember = "insert into member (conv, user, user_remote_id, user_remote_domain, service, provider, status, conversation_role) values (?, ?, ?, ?, ?, ?, 0, ?)"

removeMember :: PrepQuery W (ConvId, OpaqueUserId) ()
removeMember = "delete from member where conv = ? and user = ?"

updateOtrMemberMuted :: PrepQuery W (Bool, Maybe Text, ConvId, OpaqueUserId) ()
updateOtrMemberMuted = "update member set otr_muted = ?, otr_muted_ref = ? where conv = ? and user = ?"

updateOtrMemberMutedStatus :: PrepQuery W (MutedStatus, Maybe Text, ConvId, OpaqueUserId) ()
updateOtrMemberMutedStatus = "update member set otr_muted_status = ?, otr_muted_ref = ? where conv = ? and user = ?"

updateOtrMemberArchived :: PrepQuery W (Bool, Maybe Text, ConvId, OpaqueUserId) ()
updateOtrMemberArchived = "update member set otr_archived = ?, otr_archived_ref = ? where conv = ? and user = ?"

updateMemberHidden :: PrepQuery W (Bool, Maybe Text, ConvId, OpaqueUserId) ()
updateMemberHidden = "update member set hidden = ?, hidden_ref = ? where conv = ? and user = ?"

updateMemberConvRoleName :: PrepQuery W (RoleName, ConvId, OpaqueUserId) ()
updateMemberConvRoleName = "update member set conversation_role = ? where conv = ? and user = ?"

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

selectLegalHoldSettings :: PrepQuery R (Identity TeamId) (HttpsUrl, (Fingerprint Rsa), ServiceToken, ServiceKey)
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

-- ID Mapping ---------------------------------------------------------------

selectIdMapping :: PrepQuery R (Identity (Id (Mapped a))) (Id (Remote a), Domain)
selectIdMapping =
  "select remote_id, remote_domain from id_mapping where mapped_id = ?"

insertIdMapping :: PrepQuery W (Id (Mapped a), Id (Remote a), Domain) ()
insertIdMapping =
  "insert into id_mapping (mapped_id, remote_id, remote_domain) values (?, ?, ?)"

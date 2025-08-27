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

-- | Tables that are used in this module:
-- - billing_team_member
-- - clients
-- - conversation_codes
-- - custom_backend
-- - legalhold_pending_prekeys
-- - legalhold_service
-- - legalhold_whitelisted
-- - service
-- - team
-- - team_admin
-- - team_conv
-- - team_member
-- - user_team
-- update using: `rg -i -P '(?:update|from|into)\s+([A-Za-z0-9_]+)' -or '$1' --no-line-number services/galley/src/Galley/Cassandra/Queries.hs | sort | uniq`
module Galley.Cassandra.Queries
  ( selectCustomBackend,
    upsertCustomBackend,
    deleteCustomBackend,
    insertCode,
    lookupCode,
    deleteCode,
    upsertMemberAddClient,
    upsertMemberRmClient,
    selectClients,
    rmClients,
    selectSearchVisibility,
    updateSearchVisibility,
    insertSrv,
    selectSrv,
    rmSrv,
    insertLegalHoldSettings,
    selectLegalHoldSettings,
    removeLegalHoldSettings,
    insertPendingPrekeys,
    dropPendingPrekeys,
    selectPendingPrekeys,
    updateUserLegalHoldStatus,
    selectLegalHoldWhitelistedTeam,
    insertLegalHoldWhitelistedTeam,
    removeLegalHoldWhitelistedTeam,
    insertTeam,
    listBillingTeamMembers,
    listTeamAdmins,
    selectTeamName,
    selectTeamConv,
    selectTeamConvs,
    selectUserTeamsFrom,
    selectUserTeams,
    selectTeamMember,
    insertTeamMember,
    insertUserTeam,
    insertBillingTeamMember,
    insertTeamAdmin,
    updatePermissions,
    deleteBillingTeamMember,
    deleteTeamAdmin,
    deleteTeamMember,
    deleteUserTeam,
    selectTeam,
    selectUserTeamsIn,
    selectTeamMembers,
    selectOneUserTeam,
    selectTeamBindingWritetime,
    selectTeamBinding,
    markTeamDeleted,
    deleteTeam,
    updateTeamStatus,
    updateTeamName,
    updateTeamIcon,
    updateTeamIconKey,
    updateTeamSplashScreen,
    selectTeamConvsFrom,
    selectTeamMembersFrom,
    selectTeamMembers',
  )
where

import Cassandra as C hiding (Value)
import Cassandra.Util (Writetime)
import Data.Domain (Domain)
import Data.Id
import Data.Json.Util
import Data.LegalHold
import Data.Misc
import Data.Text.Lazy qualified as LT
import Galley.Data.Scope
import Imports
import Text.RawString.QQ
import Wire.API.Conversation.Code
import Wire.API.Password (Password)
import Wire.API.Provider
import Wire.API.Provider.Service
import Wire.API.Routes.Internal.Galley.TeamsIntra
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

insertTeamAdmin :: PrepQuery W (TeamId, UserId) ()
insertTeamAdmin = "insert into team_admin (team, user) values (?, ?)"

deleteTeamAdmin :: PrepQuery W (TeamId, UserId) ()
deleteTeamAdmin = "delete from team_admin where team = ? and user = ?"

listTeamAdmins :: PrepQuery R (Identity TeamId) (Identity UserId)
listTeamAdmins = "select user from team_admin where team = ?"

-- | This is not an upsert, but we can't add `IF EXISTS` here, or cassandra will yell `Invalid
-- "Batch with conditions cannot span multiple tables"` at us.  So we make sure in the
-- application logic to only call this if the user exists (in the handler, not entirely
-- race-condition-proof, unfortunately).
updatePermissions :: PrepQuery W (Permissions, TeamId, UserId) ()
updatePermissions = "update team_member set perms = ? where team = ? and user = ?"

insertUserTeam :: PrepQuery W (UserId, TeamId) ()
insertUserTeam = "insert into user_team (user, team) values (?, ?)"

deleteUserTeam :: PrepQuery W (UserId, TeamId) ()
deleteUserTeam = "delete from user_team where user = ? and team = ?"

markTeamDeleted :: PrepQuery W (TeamStatus, TeamId) ()
markTeamDeleted = {- `IF EXISTS`, but that requires benchmarking -} "update team set status = ? where team = ?"

deleteTeam :: PrepQuery W (TeamStatus, TeamId) ()
deleteTeam = {- `IF EXISTS`, but that requires benchmarking -} "update team using timestamp 32503680000000000 set name = 'default', icon = 'default', status = ? where team = ? "

updateTeamName :: PrepQuery W (Text, TeamId) ()
updateTeamName = {- `IF EXISTS`, but that requires benchmarking -} "update team set name = ? where team = ?"

updateTeamIcon :: PrepQuery W (Text, TeamId) ()
updateTeamIcon = {- `IF EXISTS`, but that requires benchmarking -} "update team set icon = ? where team = ?"

updateTeamIconKey :: PrepQuery W (Text, TeamId) ()
updateTeamIconKey = {- `IF EXISTS`, but that requires benchmarking -} "update team set icon_key = ? where team = ?"

updateTeamStatus :: PrepQuery W (TeamStatus, TeamId) ()
updateTeamStatus = {- `IF EXISTS`, but that requires benchmarking -} "update team set status = ? where team = ?"

updateTeamSplashScreen :: PrepQuery W (Text, TeamId) ()
updateTeamSplashScreen = {- `IF EXISTS`, but that requires benchmarking -} "update team set splash_screen = ? where team = ?"

-- Conversations accessible by code -----------------------------------------

insertCode :: PrepQuery W (Key, Value, ConvId, Scope, Maybe Password, Int32) ()
insertCode = "INSERT INTO conversation_codes (key, value, conversation, scope, password) VALUES (?, ?, ?, ?, ?) USING TTL ?"

lookupCode :: PrepQuery R (Key, Scope) (Value, Int32, ConvId, Maybe Password)
lookupCode = "SELECT value, ttl(value), conversation, password FROM conversation_codes WHERE key = ? AND scope = ?"

deleteCode :: PrepQuery W (Key, Scope) ()
deleteCode = "DELETE FROM conversation_codes WHERE key = ? AND scope = ?"

-- Clients ------------------------------------------------------------------

selectClients :: PrepQuery R (Identity [UserId]) (UserId, C.Set ClientId)
selectClients = "select user, clients from clients where user in ?"

rmClients :: PrepQuery W (Identity UserId) ()
rmClients = "delete from clients where user = ?"

upsertMemberAddClient :: ClientId -> QueryString W (Identity UserId) ()
upsertMemberAddClient c =
  let t = LT.fromStrict (clientToText c)
   in QueryString $ "update clients set clients = clients + {'" <> t <> "'} where user = ?"

upsertMemberRmClient :: ClientId -> QueryString W (Identity UserId) ()
upsertMemberRmClient c =
  let t = LT.fromStrict (clientToText c)
   in QueryString $ "update clients set clients = clients - {'" <> t <> "'} where user = ?"

-- Services -----------------------------------------------------------------

rmSrv :: PrepQuery W (ProviderId, ServiceId) ()
rmSrv = "delete from service where provider = ? AND id = ?"

insertSrv :: PrepQuery W (ProviderId, ServiceId, HttpsUrl, ServiceToken, C.Set (Fingerprint Rsa), Bool) ()
insertSrv = "insert into service (provider, id, base_url, auth_token, fingerprints, enabled) values (?, ?, ?, ?, ?, ?)"

selectSrv :: PrepQuery R (ProviderId, ServiceId) (HttpsUrl, ServiceToken, C.Set (Fingerprint Rsa), Bool)
selectSrv = "select base_url, auth_token, fingerprints, enabled from service where provider = ? AND id = ?"

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
  {- `IF EXISTS`, but that requires benchmarking -} "update team set search_visibility = ? where team = ?"

-- Custom Backend -----------------------------------------------------------

selectCustomBackend :: PrepQuery R (Identity Domain) (HttpsUrl, HttpsUrl)
selectCustomBackend =
  "select config_json_url, webapp_welcome_url from custom_backend where domain = ?"

upsertCustomBackend :: PrepQuery W (HttpsUrl, HttpsUrl, Domain) ()
upsertCustomBackend =
  "update custom_backend set config_json_url = ?, webapp_welcome_url = ? where domain = ?"

deleteCustomBackend :: PrepQuery W (Identity Domain) ()
deleteCustomBackend =
  "delete from custom_backend where domain = ?"

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Galley.Data.Queries where

import Imports
import Brig.Types.Code
import Cassandra as C hiding (Value)
import Cassandra.Util
import Data.Id
import Data.Json.Util
import Data.Misc
import Galley.Data.Types
import Galley.Types hiding (Conversation)
import Galley.Types.Bot
import Galley.Types.Teams
import Galley.Types.Teams.Intra

import qualified Data.Text.Lazy as LT

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

selectTeamMember :: PrepQuery R (TeamId, UserId) (Permissions, Maybe UserId, Maybe UTCTimeMillis)
selectTeamMember = "select perms, invited_by, invited_at from team_member where team = ? and user = ?"

selectTeamMembers :: PrepQuery R (Identity TeamId) (UserId, Permissions, Maybe UserId, Maybe UTCTimeMillis)
selectTeamMembers = "select user, perms, invited_by, invited_at from team_member where team = ? order by user"

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

-- Members ------------------------------------------------------------------

type MemberStatus = Int32

selectMember :: PrepQuery R (ConvId, UserId) (UserId, Maybe ServiceId, Maybe ProviderId, Maybe MemberStatus, Maybe Bool, Maybe MutedStatus, Maybe Text, Maybe Bool, Maybe Text, Maybe Bool, Maybe Text)
selectMember = "select user, service, provider, status, otr_muted, otr_muted_status, otr_muted_ref, otr_archived, otr_archived_ref, hidden, hidden_ref from member where conv = ? and user = ?"

selectMembers :: PrepQuery R (Identity [ConvId]) (ConvId, UserId, Maybe ServiceId, Maybe ProviderId, Maybe MemberStatus, Maybe Bool, Maybe MutedStatus, Maybe Text, Maybe Bool, Maybe Text, Maybe Bool, Maybe Text)
selectMembers = "select conv, user, service, provider, status, otr_muted, otr_muted_status, otr_muted_ref, otr_archived, otr_archived_ref, hidden, hidden_ref from member where conv in ?"

insertMember :: PrepQuery W (ConvId, UserId, Maybe ServiceId, Maybe ProviderId) ()
insertMember = "insert into member (conv, user, service, provider, status) values (?, ?, ?, ?, 0)"

removeMember :: PrepQuery W (ConvId, UserId) ()
removeMember = "delete from member where conv = ? and user = ?"

updateOtrMemberMuted :: PrepQuery W (Bool, Maybe Text, ConvId, UserId) ()
updateOtrMemberMuted = "update member set otr_muted = ?, otr_muted_ref = ? where conv = ? and user = ?"

updateOtrMemberMutedStatus :: PrepQuery W (MutedStatus, Maybe Text, ConvId, UserId) ()
updateOtrMemberMutedStatus = "update member set otr_muted_status = ?, otr_muted_ref = ? where conv = ? and user = ?"

updateOtrMemberArchived :: PrepQuery W (Bool, Maybe Text, ConvId, UserId) ()
updateOtrMemberArchived = "update member set otr_archived = ?, otr_archived_ref = ? where conv = ? and user = ?"

updateMemberHidden :: PrepQuery W (Bool, Maybe Text, ConvId, UserId) ()
updateMemberHidden = "update member set hidden = ?, hidden_ref = ? where conv = ? and user = ?"

-- Clients ------------------------------------------------------------------

selectClients :: PrepQuery R (Identity [UserId]) (UserId, C.Set ClientId)
selectClients = "select user, clients from clients where user in ?"

rmClients :: PrepQuery W (Identity UserId) ()
rmClients = "delete from clients where user = ?"

addMemberClient :: ClientId -> QueryString W (Identity UserId) ()
addMemberClient c =
    let t = LT.fromStrict (client c) in
    QueryString $ "update clients set clients = clients + {'" <> t <> "'} where user = ?"

rmMemberClient :: ClientId -> QueryString W (Identity UserId) ()
rmMemberClient c =
    let t = LT.fromStrict (client c) in
    QueryString $ "update clients set clients = clients - {'" <> t <> "'} where user = ?"

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

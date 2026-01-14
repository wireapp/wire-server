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

module Wire.TeamStore.Cassandra.Queries where

import Cassandra as C hiding (Value)
import Cassandra.Util (Writetime)
import Data.Id
import Data.Json.Util
import Data.LegalHold
import Imports
import Text.RawString.QQ
import Wire.API.Routes.Internal.Galley.TeamsIntra
import Wire.API.Team
import Wire.API.Team.Permission

-- Teams --------------------------------------------------------------------

selectTeam :: PrepQuery R (Identity TeamId) (UserId, Text, Icon, Maybe Text, Bool, Maybe TeamStatus, Maybe (Writetime TeamStatus), Maybe TeamBinding, Maybe Icon)
selectTeam = "select creator, name, icon, icon_key, deleted, status, writetime(status), binding, splash_screen from team where team = ?"

selectTeamName :: PrepQuery R (Identity TeamId) (Identity Text)
selectTeamName = "select name from team where team = ?"

selectTeamBinding :: PrepQuery R (Identity TeamId) (Identity (Maybe TeamBinding))
selectTeamBinding = "select binding from team where team = ?"

selectTeamBindingWritetime :: PrepQuery R (Identity TeamId) (Identity (Maybe Int64))
selectTeamBindingWritetime = "select writetime(binding) from team where team = ?"

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

selectTeamMembersBase :: (IsString a) => [String] -> a
selectTeamMembersBase conds = fromString $ selectFrom <> " where team = ?" <> whereClause <> " order by user"
  where
    selectFrom = "select user, perms, invited_by, invited_at, legalhold_status from team_member"
    whereClause = concatMap (" and " <>) conds

-- | This query fetches all members of a team, should be paginated.
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
selectTeamMembers = selectTeamMembersBase []

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
selectTeamMembersFrom = selectTeamMembersBase ["user > ?"]

selectTeamMembers' ::
  PrepQuery
    R
    (TeamId, [UserId])
    ( UserId,
      Permissions,
      Writetime Permissions,
      Maybe UserId,
      Maybe UTCTimeMillis,
      Maybe UserLegalHoldStatus
    )
selectTeamMembers' =
  [r|
    select user, perms, writetime(perms), invited_by, invited_at, legalhold_status
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

-- LegalHold whitelist -------------------------------------------------------

selectLegalHoldWhitelistedTeam :: PrepQuery R (Identity TeamId) (Identity TeamId)
selectLegalHoldWhitelistedTeam =
  [r|
        select team from legalhold_whitelisted where team = ?
    |]

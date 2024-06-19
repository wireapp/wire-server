-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
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

module V3_BackfillTeamAdmins where

import Cassandra
import Conduit
import Data.Conduit.Internal (zipSources)
import Data.Conduit.List qualified as C
import Data.Id
import Galley.DataMigration.Types
import Imports
import System.Logger.Class qualified as Log
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.Team.Role

migration :: Migration
migration =
  Migration
    { version = MigrationVersion 3,
      text = "Backfill team_admin",
      action =
        runConduit $
          zipSources
            (C.sourceList [(1 :: Int32) ..])
            getTeamMembers
            .| C.mapM
              ( \(i, p) ->
                  Log.info (Log.field "team members" (show (i * pageSize)))
                    >> pure p
              )
            .| C.concatMap (filter isAdmin)
            .| C.map (\(t, u, _) -> (t, u))
            .| C.mapM_ createTeamAdmins
    }

pageSize :: Int32
pageSize = 1000

----------------------------------------------------------------------------
-- Queries

-- | Get team members from Galley
getTeamMembers :: (MonadClient m) => ConduitM () [(TeamId, UserId, Maybe Permissions)] m ()
getTeamMembers = paginateC cql (paramsP LocalQuorum () pageSize) x5
  where
    cql :: PrepQuery R () (TeamId, UserId, Maybe Permissions)
    cql = "SELECT team, user, perms FROM team_member"

createTeamAdmins :: (MonadClient m) => (TeamId, UserId) -> m ()
createTeamAdmins pair =
  retry x5 $ write cql (params LocalQuorum pair)
  where
    cql :: PrepQuery W (TeamId, UserId) ()
    cql = "INSERT INTO team_admin (team, user) values (?, ?)"

isAdmin :: (TeamId, UserId, Maybe Permissions) -> Bool
isAdmin (_, _, Just p) = permissionsRole p == Just RoleAdmin || permissionsRole p == Just RoleOwner
isAdmin _ = False

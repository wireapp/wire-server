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

module V1_BackfillBillingTeamMembers where

import Cassandra
import Conduit
import Control.Lens (view)
import Data.Conduit.Internal (zipSources)
import qualified Data.Conduit.List as C
import Data.Id
import qualified Data.Set as Set
import Galley.DataMigration.Types
import Galley.Types.Teams
import Imports
import qualified System.Logger.Class as Log

migration :: Migration
migration =
  Migration
    { version = MigrationVersion 1,
      text = "Backfill billing_team_member",
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
            .| C.concatMap (filter isOwner)
            .| C.map (\(t, u, _) -> (t, u))
            .| C.chunksOf 50
            .| C.mapM
              ( \x ->
                  Log.info (Log.field "writing billing team members" (show (length x)))
                    >> pure x
              )
            .| C.mapM_ createBillingTeamMembers
    }

pageSize :: Int32
pageSize = 1000

----------------------------------------------------------------------------
-- Queries

-- | Get team members from Galley
getTeamMembers :: MonadClient m => ConduitM () [(TeamId, UserId, Maybe Permissions)] m ()
getTeamMembers = paginateC cql (paramsP Quorum () pageSize) x5
  where
    cql :: PrepQuery R () (TeamId, UserId, Maybe Permissions)
    cql = "SELECT team, user, perms FROM team_member"

createBillingTeamMembers :: MonadClient m => [(TeamId, UserId)] -> m ()
createBillingTeamMembers pairs =
  retry x5 $ batch $ do
    setType BatchLogged
    setConsistency Quorum
    mapM_ (addPrepQuery cql) pairs
  where
    cql :: PrepQuery W (TeamId, UserId) ()
    cql = "INSERT INTO billing_team_member (team, user) values (?, ?)"

isOwner :: (TeamId, UserId, Maybe Permissions) -> Bool
isOwner (_, _, Just p) = SetBilling `Set.member` view self p
isOwner _ = False

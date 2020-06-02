{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

module Work where

import Cassandra
import Cassandra.Util
import Conduit
import Control.Lens (view)
import Data.Conduit.Internal (zipSources)
import qualified Data.Conduit.List as C
import Data.Id
import qualified Data.Set as Set
import Galley.Types.Teams
import Imports
import System.Logger (Logger)
import qualified System.Logger as Log

runCommand :: Logger -> ClientState -> IO ()
runCommand l galley =
  runConduit $
    zipSources
      (C.sourceList [(1 :: Int32) ..])
      (transPipe (runClient galley) getTeamMembers)
      .| C.mapM
        ( \(i, p) ->
            Log.info l (Log.field "team members" (show (i * pageSize)))
              >> pure p
        )
      .| C.concatMap (filter isOwner)
      .| C.map (\(t, u, _, wt) -> (t, u, (writeTimeToUTC wt)))
      .| C.mapM_
        ( \x ->
            Log.info l (Log.field "..." (show (x)))
        )

pageSize :: Int32
pageSize = 1000 -- "hello"

----------------------------------------------------------------------------
-- Queries

-- | Get team members from Galley
getTeamMembers :: ConduitM () [TeamRow] Client ()
getTeamMembers = paginateC cql (paramsP Quorum () pageSize) x5
  where
    cql :: PrepQuery R () TeamRow
    cql = "SELECT team, user, perms, writetime(user) FROM team_member"

isOwner :: TeamRow -> Bool
isOwner (_, _, Just p, _) = SetBilling `Set.member` view self p
isOwner _ = False

type TeamRow = (TeamId, UserId, Maybe Permissions, Int64)

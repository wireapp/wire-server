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

module NullPermsTeamMembers where

import Cassandra
import Conduit
import Data.Aeson (ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Conduit.Internal (zipSources)
import Data.Conduit.List qualified as C
import Data.Id
import Imports
import System.Logger (Logger)
import System.Logger qualified as Log
import Wire.API.Team.Permission (Permissions)

-- | Scan galley.team_member for rows with perms = NULL and
-- enrich with user and team info. Outputs JSON lines to file.
runCommand :: Logger -> FilePath -> ClientState -> IO ()
runCommand l inconsistenciesFile casGalley = do
  runResourceT $
    runConduit $
      zipSources
        (C.sourceList [(1 :: Int32) ..])
        (transPipe (runClient casGalley) getTeamMembers)
        .| C.mapM
          ( \(i, rows) -> do
              let offenders = mapMaybe toOffender rows
              Log.info l (Log.field "rowsProcessed" (show ((i - 1) * pageSize + fromIntegral (length rows))))
              pure offenders
          )
        .| C.map ((<> "\n") . BS.intercalate "\n" . map (BS.toStrict . Aeson.encode))
        .| sinkFile inconsistenciesFile

-- | Keep only rows where perms is NULL
toOffender :: TeamMemberRow -> Maybe Offender
toOffender (tid, uid, Nothing) = Just $ Offender tid uid
toOffender _ = Nothing

data Offender = Offender
  { teamId :: TeamId,
    userId :: UserId
  }
  deriving (Generic)

instance ToJSON Offender

-- CQL helpers --------------------------------------------------------------

pageSize :: Int32
pageSize = 10000

-- | Full table scan of team_member; filter for perms = NULL in-process
getTeamMembers :: ConduitM () [TeamMemberRow] Client ()
getTeamMembers = paginateC cql (paramsP LocalQuorum () pageSize) x5
  where
    cql :: PrepQuery R () TeamMemberRow
    cql = "SELECT team, user, perms FROM team_member"

type TeamMemberRow = (TeamId, UserId, Maybe Permissions)

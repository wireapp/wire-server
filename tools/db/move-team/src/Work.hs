{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-unused-imports #-}

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

import Brig.Types hiding (Client)
import Cassandra
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit
import qualified Data.Conduit.Combinators as C
import Data.Conduit.Internal (zipSources)
import qualified Data.Conduit.List as CL
import Data.Id
import Data.Misc
import Data.Time
import Data.UUID
import Galley.Data.Instances ()
import Imports
import qualified System.IO as IO
import System.Logger (Logger)
import qualified System.Logger as Log
import UnliftIO.Async (pooledMapConcurrentlyN)
import Wire.API.Team.Feature
import Wire.API.Team.Permission

deriving instance Cql Name

pageSize :: Int32
pageSize = 100

runCommand :: Logger -> ClientState -> ClientState -> ClientState -> FilePath -> TeamId -> IO ()
runCommand lg _brig galley _spar sinkPath tid = IO.withBinaryFile sinkPath IO.WriteMode $ \outH ->
  runConduit $
    zipSources
      (CL.sourceList [(1 :: Int32) ..])
      (transPipe (runClient galley) (getTeamMembers tid))
      .| C.mapM (handleTeamMembers lg)
      .| C.mapM (pure . mconcat)
      .| sinkLines outH

sinkLines :: IO.Handle -> ConduitT LByteString Void IO ()
sinkLines hd = C.mapM_ (LBS.hPutStr hd)

data TeamMemberExport = TeamMemberExport
  { teamMemberExportTeam :: UUID,
    teamMemberExportUser :: UUID,
    teamMemberExportInvitedAt :: UTCTime,
    teamMemberExportInvitedBy :: UUID,
    teamMemberExportLegalhold :: Int,
    teamMemberExportPerms :: Permissions
  }
  deriving (Generic)

instance ToJSON TeamMemberExport

instance FromJSON TeamMemberExport

mkTeamMemberExport :: (UUID, UUID, UTCTime, UUID, Int32) -> TeamMemberExport
mkTeamMemberExport (team, user, invitedAt, invitedBy, legalhold) =
  TeamMemberExport team user invitedAt invitedBy (fromIntegral legalhold) (error "perms")

getTeamMembers :: TeamId -> ConduitM () [TeamMemberExport] Client ()
getTeamMembers tid = paginateC cql (paramsP Quorum (pure tid) pageSize) x5 .| C.mapM (pure . fmap mkTeamMemberExport)
  where
    cql :: PrepQuery R (Identity TeamId) (UUID, UUID, UTCTime, UUID, Int32)
    cql = "select team, user, invited_at, invited_by, legalhold_status from team_member where team=?"

handleTeamMembers :: Logger -> (Int32, [TeamMemberExport]) -> IO [LByteString]
handleTeamMembers lg (i, members) = do
  Log.info lg (Log.field "number of team members loaded: " (show (i * pageSize)))
  pure $ (encode <$> members)

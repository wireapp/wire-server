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
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import qualified System.IO as IO
import System.Logger (Logger)
import qualified System.Logger as Log
import System.Process (system)
import UnliftIO.Async (pooledMapConcurrentlyN)
import Wire.API.Team.Feature
import Wire.API.Team.Permission

deriving instance Cql Name

pageSize :: Int32
pageSize = 100

runCommand :: Logger -> ClientState -> ClientState -> ClientState -> FilePath -> TeamId -> IO ()
runCommand lg _brig galley _spar sinkPath tid = do
  ExitSuccess <- system $ "mkdir -p " <> show sinkPath
  IO.withBinaryFile (sinkPath </> "galley.team_member") IO.WriteMode $ \outH ->
    runConduit $
      zipSources
        (CL.sourceList [(1 :: Int32) ..])
        (transPipe (runClient galley) (getTeamMembers tid))
        .| C.mapM (handleTeamMembers lg)
        .| C.mapM (pure . mconcat)
        .| sinkLines outH

sinkLines :: IO.Handle -> ConduitT LByteString Void IO ()
sinkLines hd = C.mapM_ (LBS.hPutStr hd)

type TeamMemberRow = (UUID, UUID, Maybe UTCTime, Maybe UUID, Maybe Int32, Permissions)

getTeamMembers :: TeamId -> ConduitM () [TeamMemberRow] Client ()
getTeamMembers tid = paginateC cql (paramsP Quorum (pure tid) pageSize) x5
  where
    cql :: PrepQuery R (Identity TeamId) TeamMemberRow
    cql = "select team, user, invited_at, invited_by, legalhold_status, perms from team_member where team=?"

handleTeamMembers :: Logger -> (Int32, [TeamMemberRow]) -> IO [LByteString]
handleTeamMembers lg (i, members) = do
  Log.info lg (Log.field "number of team members loaded: " (show (i * pageSize)))

  pure $ (encode <$> members)

--
--
--
--

-- one file per table, only use tuples.

-- dump-selected-query.

-- one big problem: needs re-login (cookies won't need to get moved).

-- old assets may be available from the new backend since they're global.  so clients can fetch both.  possibly.

-- instead of writing putTeamMembers, perhaps we can write cql code to the file instead of
-- tuples?  and have getTeamMembers return [Text] rather than [TeamMemberExport]?  yes, that
-- seems like a good idea.

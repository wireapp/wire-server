{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
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
import Control.Lens (view, _2)
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

data Env = Env
  { envLogger :: Logger,
    envBrig :: ClientState,
    envGalley :: ClientState,
    envSpar :: ClientState,
    envTargetPath :: FilePath,
    envTeamId :: TeamId
  }

runCommand :: Env -> IO ()
runCommand env@Env {..} = do
  ExitSuccess <- system $ "mkdir -p " <> show envTargetPath
  runGalleyTeamMembers env
  -- runGalleyTeamConv env

----------------------------------------------------------------------

runGalleyTeamMembers :: Env -> IO ()
runGalleyTeamMembers env@Env {..} = do
  IO.withBinaryFile (envTargetPath </> "galley.team_member") IO.WriteMode $ \outH ->
    runConduit $
      zipSources
        (CL.sourceList [(1 :: Int32) ..])
        (transPipe (runClient envGalley) (readGalleyTeamMember envTeamId))
        .| C.mapM (handleTeamMembers env)
        .| C.mapM (pure . mconcat)
        .| sinkLines outH

-- TODO: this should somehow get generated.
type TeamMemberRow = (UUID, UUID, Maybe UTCTime, Maybe UUID, Maybe Int32, Permissions)

-- TODO: this should somehow get generated.
readGalleyTeamMember :: TeamId -> ConduitM () [TeamMemberRow] Client ()
readGalleyTeamMember tid = paginateC cql (paramsP Quorum (pure tid) pageSize) x5
  where
    cql :: PrepQuery R (Identity TeamId) TeamMemberRow
    cql = "select team, user, invited_at, invited_by, legalhold_status, perms from team_member where team=?"

handleTeamMembers :: Env -> (Int32, [TeamMemberRow]) -> IO [LByteString]
handleTeamMembers env@Env {..} (i, members) = do
  Log.info envLogger (Log.field "number of team members loaded: " (show (i * pageSize)))
  writeToFile env "galley.clients" (readGalleyClients (view _2 <$> members))

  -- ...

  pure $ (encode <$> members) -- (nit-pick TODO: this could be implicit, done in the pipeline somehow.)

-- TODO: this should somehow get generated.

type ClientsRow = (UUID, ())

-- TODO: this should somehow get generated.
readGalleyClients :: [UUID] -> Client [ClientsRow]
readGalleyClients = undefined

----------------------------------------------------------------------

runTeamConv :: Env -> IO ()
runTeamConv env@Env {..} = do
  IO.withBinaryFile (envTargetPath </> "galley.team_conv") IO.WriteMode $ \outH ->
    runConduit $
      zipSources
        (CL.sourceList [(1 :: Int32) ..])
        (transPipe (runClient envGalley) (readGalleyTeamConv envTeamId))
        .| C.mapM (handleTeamConv env)
        .| C.mapM (pure . mconcat)
        .| sinkLines outH

-- TODO: this should somehow get generated.
readGalleyTeamConv :: a
readGalleyTeamConv = undefined

handleTeamConv :: a
handleTeamConv = undefined

-- ...

----------------------------------------------------------------------
-- helpers (TODO: move to separate module)

sinkLines :: IO.Handle -> ConduitT LByteString Void IO ()
sinkLines hd = C.mapM_ (LBS.hPutStr hd)

writeToFile :: ToJSON a => Env -> FilePath -> Client a -> IO ()
writeToFile = undefined

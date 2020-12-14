{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
import Common
import Control.Lens (view, _2)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit
import Data.Conduit.Combinators (linesUnbounded, linesUnboundedAscii)
import qualified Data.Conduit.Combinators as C
import Data.Conduit.Internal (zipSources)
import qualified Data.Conduit.List as CL
import Data.Id
import Data.Misc
import qualified Data.Text as T
import Data.Time
import Data.UUID
import qualified Data.Vector as V
import Database.CQL.Protocol (Query (Query), Tuple)
import Galley.Data.Instances ()
import Imports
import Schema
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.FilePath ((</>))
import System.IO
import qualified System.IO as IO
import System.Logger (Logger)
import qualified System.Logger as Log
import System.Process (system)
import Types
import UnliftIO.Async (pooledMapConcurrentlyN)
import Wire.API.Team.Feature
import Wire.API.Team.Permission

deriving instance Cql Name -- TODO

assertTargetDirEmpty :: Env -> IO ()
assertTargetDirEmpty Env {..} = do
  files <- listDirectory envTargetPath
  unless (Imports.null files) $ do
    IO.putStrLn $ "Target directory " <> envTargetPath <> " is not empty."
    exitWith (ExitFailure 1)

runExport :: Env -> IO ()
runExport env@Env {..} = do
  ExitSuccess <- system $ "mkdir -p " <> show envTargetPath
  assertTargetDirEmpty env
  runGalleyTeamMembers env
  runGalleyTeamConv env

runImport :: Env -> IO ()
runImport env = do
  IO.putStrLn "runImport"
  importAllTables env

runDebugExportFull :: Env -> IO ()
runDebugExportFull env = do
  assertTargetDirEmpty env
  exportAllTablesFull env

----------------------------------------------------------------------

runGalleyTeamMembers :: Env -> IO ()
runGalleyTeamMembers env@Env {..} =
  IO.withBinaryFile (envTargetPath </> "galley.team_member") IO.WriteMode $ \outH ->
    runConduit $
      zipSources
        (CL.sourceList [(1 :: Int32) ..])
        (readGalleyTeamMember env envTeamId)
        .| C.mapM (handleTeamMembers env)
        .| sinkJsonLines outH

handleTeamMembers :: Env -> (Int32, [RowGalleyTeamMember]) -> IO [RowGalleyTeamMember]
handleTeamMembers env@Env {..} (i, members) = do
  Log.info envLogger (Log.field "number of team members loaded: " (show (i * envPageSize)))
  let uids = catMaybes $ fmap Id . view _2 <$> members

  appendJsonLines (envTargetPath </> "brig.clients") (readBrigClients env uids)

  pure members -- (nit-pick TODO: this could be implicit, done in the pipeline somehow.)

----------------------------------------------------------------------

runGalleyTeamConv :: Env -> IO ()
runGalleyTeamConv env@Env {..} =
  IO.withBinaryFile (envTargetPath </> "galley.team_conv") IO.WriteMode $ \outH ->
    runConduit $
      zipSources
        (CL.sourceList [(1 :: Int32) ..])
        (readGalleyTeamConv env envTeamId)
        .| C.mapM (handleTeamConv env)
        .| sinkJsonLines outH

handleTeamConv :: Env -> (Int32, [RowGalleyTeamConv]) -> IO [RowGalleyTeamConv]
handleTeamConv Env {..} (i, convs) = do
  Log.info envLogger (Log.field "number of team convs loaded: " (show (i * envPageSize)))
  pure convs

appendJsonLines :: ToJSON a => FilePath -> ConduitM () [a] IO () -> IO ()
appendJsonLines path conduit =
  IO.withBinaryFile path IO.AppendMode $ \outH ->
    runConduit $ conduit .| sinkJsonLines outH

----------------------------------------------------------------------
-- helpers (TODO: move to separate module)

--   writeToFile env "galley.clients" (readGalleyClients env uids)

-- writeToFile :: ToJSON a => Env -> FilePath -> IO [a] -> IO ()
-- writeToFile Env {..} tableFile getter =
--   Imports.withFile (envTargetPath </> tableFile) AppendMode $ \hd ->
--     mapM_ (LBS.hPutStr hd . (<> "\n") . encode) =<< getter

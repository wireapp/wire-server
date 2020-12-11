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
import Data.IP (IP (..))
import Data.Id
import Data.Misc
import qualified Data.Text as T
import Data.Time
import Data.UUID
import qualified Data.Vector as V
import Galley.Data.Instances ()
import Imports
import Schema
import System.Exit (ExitCode (ExitSuccess))
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

runCommand :: Env -> IO ()
runCommand env@Env {..} = do
  ExitSuccess <- system $ "mkdir -p " <> show envTargetPath
  runGalleyTeamMembers env
  runGalleyTeamConv env

----------------------------------------------------------------------

runGalleyTeamMembers :: Env -> IO ()
runGalleyTeamMembers env@Env {..} = do
  IO.withBinaryFile (envTargetPath </> "galley.team_member") IO.WriteMode $ \outH ->
    runConduit $
      zipSources
        (CL.sourceList [(1 :: Int32) ..])
        (readGalleyTeamMemberConduit env envTeamId)
        .| C.mapM (handleTeamMembers env)
        .| sinkLines outH

handleTeamMembers :: Env -> (Int32, [RowGalleyTeamMember]) -> IO [RowGalleyTeamMember]
handleTeamMembers env@Env {..} (i, members) = do
  Log.info envLogger (Log.field "number of team members loaded: " (show (i * envPageSize)))
  writeToFile env "galley.clients" (readGalleyClients env (catMaybes $ (fmap Id . view _2) <$> members))

  -- ...

  pure members -- (nit-pick TODO: this could be implicit, done in the pipeline somehow.)

----------------------------------------------------------------------

runGalleyTeamConv :: Env -> IO ()
runGalleyTeamConv env@Env {..} = do
  IO.withBinaryFile (envTargetPath </> "galley.team_conv") IO.WriteMode $ \outH ->
    runConduit $
      zipSources
        (CL.sourceList [(1 :: Int32) ..])
        (readGalleyTeamConvConduit env envTeamId)
        .| C.mapM (handleTeamConv env)
        .| sinkLines outH

handleTeamConv :: Env -> (Int32, [RowGalleyTeamConv]) -> IO [RowGalleyTeamConv]
handleTeamConv Env {..} (i, convs) = do
  Log.info envLogger (Log.field "number of team convs loaded: " (show (i * envPageSize)))
  pure convs

-- ...

----------------------------------------------------------------------
-- helpers (TODO: move to separate module)

sinkLines :: ToJSON a => IO.Handle -> ConduitT [a] Void IO ()
sinkLines hd = C.mapM_ (mapM_ (LBS.hPutStr hd . (<> "\n") . encode))

writeToFile :: ToJSON a => Env -> FilePath -> IO [a] -> IO ()
writeToFile Env {..} tableFile getter = do
  Imports.withFile (envTargetPath </> tableFile) AppendMode $ \hd ->
    mapM_ (LBS.hPutStr hd . (<> "\n") . encode) =<< getter

instance ToJSON IP where
  toJSON ip = String (T.pack . show $ ip)

instance FromJSON IP where
  parseJSON = withText "IP" $ \str ->
    case (read . T.unpack) str of
      Nothing -> fail "not a formatted IP address"
      Just ip -> pure ip

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Common
import Conduit (mapC, takeWhileCE)
import Control.Lens (view, _2)
import Data.Aeson
import Data.ByteString.Internal (c2w)
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

runExport :: Env -> IO ()
runExport env@Env {..} = do
  ExitSuccess <- system $ "mkdir -p " <> show envTargetPath
  -- runGalleyTeamMembers env
  -- runGalleyTeamConv env
  runFullBackup env

----------------------------------------------------------------------

runGalleyTeamMembers :: Env -> IO ()
runGalleyTeamMembers env@Env {..} =
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
  writeToFile env "galley.clients" (readGalleyClients env (catMaybes $ fmap Id . view _2 <$> members))

  -- ...

  pure members -- (nit-pick TODO: this could be implicit, done in the pipeline somehow.)

----------------------------------------------------------------------

runGalleyTeamConv :: Env -> IO ()
runGalleyTeamConv env@Env {..} =
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
writeToFile Env {..} tableFile getter =
  Imports.withFile (envTargetPath </> tableFile) AppendMode $ \hd ->
    mapM_ (LBS.hPutStr hd . (<> "\n") . encode) =<< getter

-- TODO(stefan): refactor into into template
runFullBackup :: Env -> IO ()
runFullBackup env@Env {..} = do
  exportConduit env readBrigClientsConduitAll "brig.clients"
  exportConduit env readBrigConnectionConduitAll "brig.connection"
  exportConduit env readBrigIdMappingConduitAll "brig.id_mapping"
  exportConduit env readBrigLoginCodesConduitAll "brig.login_codes"
  exportConduit env readBrigPasswordResetConduitAll "brig.password_reset"
  exportConduit env readBrigPrekeysConduitAll "brig.prekeys"
  exportConduit env readBrigPropertiesConduitAll "brig.properties"
  exportConduit env readBrigRichInfoConduitAll "brig.rich_info"
  exportConduit env readBrigUserConduitAll "brig.user"
  exportConduit env readBrigUserHandleConduitAll "brig.user_handle"
  exportConduit env readGalleyBillingTeamMemberConduitAll "galley.billing_team_member"
  exportConduit env readGalleyClientsConduitAll "galley.clients"
  exportConduit env readGalleyConversationConduitAll "galley.conversation"
  exportConduit env readGalleyMemberConduitAll "galley.member"
  exportConduit env readGalleyTeamConduitAll "galley.team"
  exportConduit env readGalleyTeamConvConduitAll "galley.team_conv"
  exportConduit env readGalleyTeamFeaturesConduitAll "galley.team_features"
  exportConduit env readGalleyTeamMemberConduitAll "galley.team_member"
  exportConduit env readGalleyTeamNotificationsConduitAll "galley.team_notifications"
  exportConduit env readGalleyUserConduitAll "galley.user"
  exportConduit env readGalleyUserTeamConduitAll "galley.user_team"
  exportConduit env readGundeckNotificationsConduitAll "gundeck.notifications"
  where
    exportConduit :: ToJSON a => Env -> (Env -> ConduitM () [a] IO ()) -> String -> IO ()
    exportConduit env' loader tablename = do
      IO.putStrLn tablename
      IO.withBinaryFile (envTargetPath </> tablename) IO.WriteMode $ \handle ->
        runConduit $
          loader env'
            .| sinkLines handle

insert :: PrepQuery W RowGalleyUserTeam ()
insert = "INSERT INTO user_team (user, team) VALUES (?, ?)"

-- TODO: connect to sinks in Schema.hs & refactor into template
runImport :: Env -> IO ()
runImport env = do
  IO.putStrLn "runImport"
  importAllTables env

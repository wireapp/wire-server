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
  -- runGalleyTeamMembers env
  -- runGalleyTeamConv env
  runFullBackup env

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

runFullBackup :: Env -> IO ()
runFullBackup env@Env {..} = do
  export env readBrigClientsAll "brig.clients"
  export env readBrigConnectionAll "brig.connection"
  export env readBrigIdMappingAll "brig.id_mapping"
  export env readBrigLoginCodesAll "brig.login_codes"
  export env readBrigPasswordResetAll "brig.password_reset"
  export env readBrigPrekeysAll "brig.prekeys"
  export env readBrigPropertiesAll "brig.properties"
  export env readBrigRichInfoAll "brig.rich_info"
  export env readBrigUserAll "brig.user"
  export env readBrigUserHandleAll "brig.user_handle"
  export env readGalleyBillingTeamMemberAll "galley.billing_team_member"
  export env readGalleyClientsAll "galley.clients"
  export env readGalleyConversationAll "galley.conversation"
  export env readGalleyMemberAll "galley.member"
  export env readGalleyTeamAll "galley.team"
  export env readGalleyTeamConvAll "galley.team_conv"
  export env readGalleyTeamFeaturesAll "galley.team_features"
  export env readGalleyTeamMemberAll "galley.team_member"
  export env readGalleyTeamNotificationsAll "galley.team_notifications"
  export env readGalleyUserAll "galley.user"
  export env readGalleyUserTeamAll "galley.user_team"
  export env readGundeckNotificationsAll "gundeck.notifications"
  export env readSparScimUserTimesAll "spar.scim_user_times"

export :: (Foldable t, ToJSON a) => Env -> (Env -> IO (t a)) -> FilePath -> IO ()
export env loader tablename =
  IO.withBinaryFile (envTargetPath env </> tablename) IO.WriteMode $ \handle -> do
    loader env >>= mapM_ (LBS.hPutStr handle . (<> "\n") . encode)

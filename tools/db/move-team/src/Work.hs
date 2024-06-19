{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-unused-imports #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

import Cassandra hiding (Set)
import Common
import Conduit (mapC)
import Control.Lens (view, _2, _3, _5)
import Data.Aeson
import Data.Conduit
import Data.Conduit.Combinators qualified as C
import Data.Conduit.Internal (zipSources)
import Data.Conduit.List qualified as CL
import Data.Id
import Data.Set qualified as Set
import Data.UUID
import Galley.Cassandra.Instances ()
import Imports
import Schema
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.FilePath ((</>))
import System.IO qualified as IO
import System.Logger qualified as Log
import System.Process (system)
import Types

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
  runTeam env
  runGalleyTeamMembers env
  runGalleyTeamConv env
  readAllTeamUserIds env >>= runFullScans env

runImport :: Env -> IO ()
runImport env = do
  IO.putStrLn "runImport"
  importAllTables env

-- FUTUREWORK: all this debug stuff should go into a separate module.
runDebugExportFull :: Env -> IO ()
runDebugExportFull env = do
  assertTargetDirEmpty env
  exportAllTablesFull env

----------------------------------------------------------------------

runTeam :: Env -> IO ()
runTeam env@Env {..} = do
  appendJsonLines (envTargetPath </> "galley.billing_team_member") (readGalleyBillingTeamMember env envTeamId)
  appendJsonLines (envTargetPath </> "galley.team") (readGalleyTeam env envTeamId)
  appendJsonLines (envTargetPath </> "galley.team_features") (readGalleyTeamFeatures env envTeamId)
  appendJsonLines (envTargetPath </> "galley.team_member") (readGalleyTeamMember env envTeamId)
  appendJsonLines (envTargetPath </> "galley.team_notifications") (readGalleyTeamNotifications env envTeamId)
  appendJsonLines (envTargetPath </> "spar.scim_external") (readSparScimExternal env envTeamId)

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
  let uids = mapMaybe (fmap Id . view _2) members

  appendJsonLines (envTargetPath </> "brig.clients") (readBrigClients env uids)
  appendJsonLines (envTargetPath </> "brig.connection") (readBrigConnection env uids)
  appendJsonLines (envTargetPath </> "brig.login_codes") (readBrigLoginCodes env uids)
  appendJsonLines (envTargetPath </> "brig.prekeys") (readBrigPrekeys env uids)
  appendJsonLines (envTargetPath </> "brig.properties") (readBrigProperties env uids)
  appendJsonLines (envTargetPath </> "brig.rich_info") (readBrigRichInfo env uids)
  appendJsonLines (envTargetPath </> "brig.user") (readBrigUser env uids)
  appendJsonLines (envTargetPath </> "galley.clients") (readGalleyClients env uids)
  appendJsonLines (envTargetPath </> "galley.user") (readGalleyUser env uids)
  appendJsonLines (envTargetPath </> "galley.user_team") (readGalleyUserTeam env uids)
  appendJsonLines (envTargetPath </> "gundeck.notifications") (readGundeckNotifications env uids)
  appendJsonLines (envTargetPath </> "spar.scim_user_times") (readSparScimUserTimes env uids)

  pure members -- (nit-pick FUTUREWORK: this could be implicit, done in the pipeline somehow.)

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
handleTeamConv env@Env {..} (i, convs) = do
  Log.info envLogger (Log.field "number of team convs loaded: " (show (i * envPageSize)))
  let cids = mapMaybe (fmap Id . view _2) convs
  appendJsonLines (envTargetPath </> "galley.conversation") (readGalleyConversation env cids)
  appendJsonLines (envTargetPath </> "galley.member") (readGalleyMember env cids)
  pure convs

runFullScans :: Env -> Set UUID -> IO ()
runFullScans env@Env {..} users = do
  let haveId Nothing = False
      haveId (Just uuid) = uuid `Set.member` users

  -- FUTUREWORK: this data likely won't work after the import on the new instance, and it's not needed.
  appendJsonLines (envTargetPath </> "brig.password_reset") $
    readBrigPasswordResetAll env
      .| mapC (filter (haveId . view _5))

  -- FUTUREWORK: no need to read this table, it can be populated from `brig.user`
  appendJsonLines (envTargetPath </> "brig.user_handle") $
    readBrigUserHandleAll env
      .| mapC (filter (haveId . view _2))

  -- FUTUREWORK: no need to read this table, it can be populated from `brig.user`
  appendJsonLines (envTargetPath </> "brig.user_keys") $
    readBrigUserKeysAll env
      .| mapC (filter (haveId . view _2))

  appendJsonLines (envTargetPath </> "spar.user") $
    readSparUserAll env
      .| mapC (filter (haveId . view _3))

appendJsonLines :: (ToJSON a) => FilePath -> ConduitM () [a] IO () -> IO ()
appendJsonLines path conduit =
  IO.withBinaryFile path IO.AppendMode $ \outH ->
    runConduit $ conduit .| sinkJsonLines outH

readAllTeamUserIds :: Env -> IO (Set UUID)
readAllTeamUserIds env@Env {..} =
  runConduit $
    readGalleyTeamMember env envTeamId
      .| foldSetUserIds
  where
    foldSetUserIds :: ConduitM [RowGalleyTeamMember] Void IO (Set UUID)
    foldSetUserIds = C.foldl (foldl' insertUserId) Set.empty

    insertUserId :: Set UUID -> RowGalleyTeamMember -> Set UUID
    insertUserId m row = maybe m (`Set.insert` m) (view _2 row)

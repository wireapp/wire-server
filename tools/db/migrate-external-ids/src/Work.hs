{-# OPTIONS_GHC -Wno-orphans -Wno-unused-imports -Wno-name-shadowing #-}

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
import qualified Cassandra as C
import qualified Cassandra.Settings as C
import Control.Lens
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Data.Conduit.Internal (zipSources)
import qualified Data.Conduit.List as CL
import Data.Functor.Identity
import Data.Id
import qualified Data.Map.Strict as Map
import Data.Misc
import Database.CQL.Protocol (Tuple)
import Imports
import Options
import Options.Applicative
import System.Logger (Logger)
import qualified System.Logger as Log
import UnliftIO.Async (pooledMapConcurrentlyN)
import Wire.API.Team.Feature

data Env = Env
  { envBrig :: ClientState,
    envSpar :: ClientState,
    envLogger :: Logger,
    envPageSize :: Int32,
    envDebug :: Debug,
    envDryDrun :: DryRun,
    envFailCount :: IORef Int
  }

main :: IO ()
main = do
  s <- execParser (info (helper <*> settingsParser) desc)
  lgr <- initLogger s
  brig <- initCas (s ^. setCasBrig) lgr
  spar <- initCas (s ^. setCasSpar) lgr
  failCount <- newIORef 0
  let env = Env brig spar lgr (s ^. setPageSize) (s ^. setDebug) (s ^. setDryRun) failCount
  runCommand env
  where
    desc =
      header "migrate-external-ids"
        <> progDesc "Migrate from spar."
        <> fullDesc
    initLogger s =
      Log.new
        . Log.setOutput Log.StdOut
        . Log.setFormat Nothing
        . Log.setBufSize 0
        . Log.setLogLevel
          ( case s ^. setDebug of
              Debug -> Log.Debug
              _ -> Log.Warn
          )
        $ Log.defSettings
    initCas cas l =
      C.init
        . C.setLogger (C.mkLogger l)
        . C.setContacts (cas ^. cHosts) []
        . C.setPortNumber (fromIntegral $ cas ^. cPort)
        . C.setKeyspace (cas ^. cKeyspace)
        . C.setProtocolVersion C.V4
        $ C.defSettings

runCommand :: Env -> IO ()
runCommand env@Env {..} = do
  runConduit $
    zipSources
      (CL.sourceList [(1 :: Int32) ..])
      (readLegacyExternalIds env)
      .| CC.mapM (resolveTeam env)
      .| sinkMain env

  failCount <- readIORef envFailCount
  when (failCount > 0) $
    Log.warn envLogger (Log.msg @String (show failCount <> " external ids have *NOT* been migrated.\nAn external id fails to be migrated if the mapped user doesn't exist or doesn't have a team."))

type LegacyExternalId = (Text, UserId)

type UserTeam = (UserId, Maybe TeamId)

type NewExternalId = (TeamId, Text, UserId)

data ResolveTeamResult
  = UserHasNoTeam UserId Text
  | NewExternalId NewExternalId

readLegacyExternalIds :: Env -> ConduitM () [LegacyExternalId] IO ()
readLegacyExternalIds Env {..} =
  transPipe (runClient envSpar) $
    paginateC select (paramsP Quorum () envPageSize) x5
  where
    select :: PrepQuery R () LegacyExternalId
    select = "SELECT external, user FROM scim_external_ids"

resolveTeam :: Env -> (Int32, [LegacyExternalId]) -> IO [ResolveTeamResult]
resolveTeam env@Env {..} (page, exts) = do
  Log.info envLogger (Log.msg @String ("Processing page " <> show page))
  userToTeam <- Map.fromList <$> readUserTeam env (fmap snd exts)
  pure $
    exts <&> \(extid, uid) ->
      case uid `Map.lookup` userToTeam of
        Just (Just tid) -> NewExternalId (tid, extid, uid)
        _ -> UserHasNoTeam uid extid
  where
    readUserTeam :: Env -> [UserId] -> IO [UserTeam]
    readUserTeam Env {..} uids =
      runClient envBrig $ do
        query select (params Quorum (Identity uids))
      where
        select :: PrepQuery R (Identity [UserId]) UserTeam
        select = "SELECT id, team FROM user where id in ?"

isDebug :: Debug -> Bool
isDebug Debug = True
isDebug NoDebug = False

sinkMain :: Env -> ConduitM [ResolveTeamResult] Void IO ()
sinkMain Env {..} = go
  where
    go = do
      mbResult <- await
      for_ mbResult $ \results -> do
        for_ results $ \case
          UserHasNoTeam uid extid -> do
            modifyIORef envFailCount (+ 1)
            when (isDebug envDebug) $
              Log.debug envLogger (Log.msg @String ("No team for user " <> show uid <> " from extid " <> show extid))
          NewExternalId (tid, extid, uid) -> do
            case envDryDrun of
              DryRun -> pure ()
              NoDryRun ->
                runClient envSpar $
                  write insert (params Quorum (tid, extid, uid))
        go
    insert :: PrepQuery W (TeamId, Text, UserId) ()
    insert = "INSERT INTO scim_external (team, external_id, user) VALUES (?, ?, ?)"

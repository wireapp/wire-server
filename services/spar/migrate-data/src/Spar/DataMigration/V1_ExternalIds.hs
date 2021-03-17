{-# LANGUAGE RecordWildCards #-}

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

module Spar.DataMigration.V1_ExternalIds where

import Cassandra
import qualified Cassandra as C
import Control.Lens
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Data.Conduit.Internal (zipSources)
import qualified Data.Conduit.List as CL
import Data.Id
import qualified Data.Map.Strict as Map
import Imports
import Spar.DataMigration.RIO (RIO (..), modifyRef, readRef, runRIO)
import Spar.DataMigration.Types hiding (logger)
import qualified Spar.DataMigration.Types as Types
import System.Logger (Logger)
import qualified System.Logger as Log

migration :: Migration
migration =
  Migration
    { version = MigrationVersion 1,
      text = "Backfill spar.scim_external",
      action = migrationAction
    }

type LegacyExternalId = (Text, UserId)

type UserTeam = (UserId, Maybe TeamId)

type NewExternalId = (TeamId, Text, UserId)

data ResolveTeamResult
  = UserHasNoTeam UserId Text
  | NewExternalId NewExternalId

---------------------------------------------------------------------------------

class HasMigEnv env where
  migEnv :: env -> Env

askMigEnv :: HasMigEnv env => RIO env Env
askMigEnv = asks migEnv

class HasFailCount env where
  failCount :: env -> IORef Int32

class HasLogger env where
  logger :: env -> Logger

logDebug :: HasLogger env => String -> RIO env ()
logDebug msg = do
  log_ :: Logger <- asks logger
  Log.debug log_ (Log.msg msg)

logWarn :: HasLogger env => String -> RIO env ()
logWarn msg = do
  log_ :: Logger <- asks logger
  Log.warn log_ (Log.msg msg)

class HasSpar env where
  sparClientState :: env -> C.ClientState

runSpar :: HasSpar env => Client a -> RIO env a
runSpar cl = do
  cs <- asks sparClientState
  runClient cs cl

class HasBrig env where
  brigClientState :: env -> C.ClientState

runBrig :: HasBrig env => Client a -> RIO env a
runBrig cl = do
  cs <- asks brigClientState
  runClient cs cl

---------------------------------------------------------------------------------

data V1Env = V1Env {v1FailCount :: IORef Int32, migrationEnv :: Env}

instance HasSpar V1Env where
  sparClientState = sparCassandra . migrationEnv

instance HasBrig V1Env where
  brigClientState = brigCassandra . migrationEnv

instance HasLogger V1Env where
  logger = Types.logger . migrationEnv

instance HasMigEnv V1Env where
  migEnv = migrationEnv

instance HasFailCount V1Env where
  failCount = v1FailCount

migrationAction :: Env -> IO ()
migrationAction migrationEnv = do
  v1FailCount <- newIORef 0
  let v1env = V1Env {..}
  runRIO v1env migrationMain

migrationMain ::
  ( HasSpar env,
    HasBrig env,
    HasLogger env,
    HasMigEnv env,
    HasFailCount env
  ) =>
  RIO env ()
migrationMain = do
  runConduit $
    zipSources
      (CL.sourceList [(1 :: Int32) ..])
      readLegacyExternalIds
      .| CC.mapM resolveTeam
      .| sink

  count <- readRef failCount
  when (count > 0) $
    logWarn (show count <> " external ids have *NOT* been migrated.\nAn external id fails to be migrated if the mapped user doesn't exist or doesn't have a team.")

readLegacyExternalIds :: (HasSpar env, HasMigEnv env) => ConduitM () [LegacyExternalId] (RIO env) ()
readLegacyExternalIds = do
  pSize <- lift $ pageSize <$> askMigEnv
  transPipe runSpar $
    paginateC select (paramsP Quorum () pSize) x5
  where
    select :: PrepQuery R () LegacyExternalId
    select = "SELECT external, user FROM scim_external_ids"

resolveTeam :: (HasLogger env, HasBrig env) => (Int32, [LegacyExternalId]) -> RIO env [ResolveTeamResult]
resolveTeam (page, exts) = do
  userToTeam <- Map.fromList <$> readUserTeam (fmap snd exts)
  logDebug $ "Page " <> show page
  pure $
    exts <&> \(extid, uid) ->
      case uid `Map.lookup` userToTeam of
        Just (Just tid) -> NewExternalId (tid, extid, uid)
        _ -> UserHasNoTeam uid extid
  where
    readUserTeam :: HasBrig env => [UserId] -> RIO env [UserTeam]
    readUserTeam uids =
      runBrig $ do
        query select (params Quorum (Identity uids))
      where
        select :: PrepQuery R (Identity [UserId]) UserTeam
        select = "SELECT id, team FROM user where id in ?"

sink ::
  ( HasSpar env,
    HasLogger env,
    HasMigEnv env,
    HasFailCount env
  ) =>
  ConduitM [ResolveTeamResult] Void (RIO env) ()
sink = go
  where
    go = do
      mbResult <- await
      for_ mbResult $ \results -> do
        for_ results $ \case
          UserHasNoTeam uid extid -> do
            lift $ do
              modifyRef failCount (+ 1)
              dbg <- debug <$> askMigEnv
              when (dbg == Debug) $
                logDebug ("No team for user " <> show uid <> " from extid " <> show extid)
          NewExternalId (tid, extid, uid) ->
            lift $
              dryRun <$> askMigEnv >>= \case
                DryRun -> pure ()
                NoDryRun ->
                  runSpar $
                    write insert (params Quorum (tid, extid, uid))
        go
    insert :: PrepQuery W (TeamId, Text, UserId) ()
    insert = "INSERT INTO scim_external (team, external_id, user) VALUES (?, ?, ?)"

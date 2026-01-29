{-# LANGUAGE RecordWildCards #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.TeamFeatureStore.Migration where

import Cassandra hiding (Value)
import Data.ByteString.Conversion
import Data.Conduit
import Data.Conduit.List qualified as C
import Data.Id
import Hasql.Pool qualified as Hasql
import Imports
import Polysemy
import Polysemy.Async
import Polysemy.Conc
import Polysemy.Error
import Polysemy.Input
import Polysemy.State
import Polysemy.Time
import Polysemy.TinyLog
import Prometheus qualified
import System.Logger qualified as Log
import Wire.API.Team.Feature
import Wire.Migration hiding (handleErrors)
import Wire.MigrationLock
import Wire.Postgres
import Wire.Sem.Logger (mapLogger)
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)
import Wire.TeamFeatureStore.Cassandra.Queries qualified as Cql
import Wire.TeamFeatureStore.Postgres.Queries qualified as Psql

migrateAllTeamFeatures ::
  ( Member (Input Hasql.Pool) r,
    Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r,
    Member (State Int) r,
    Member Async r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  ConduitM () Void (Sem r) ()
migrateAllTeamFeatures migOpts migCounter = do
  lift $ info $ Log.msg (Log.val "migrateAllTeamFeatures  ")
  withCount (paginateSem Cql.selectAll (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| C.mapM_ (traverse_ (\row@(tid, feat, _, _, _) -> handleErrors (toByteString' (idToText tid <> " - " <> feat)) (migrateTeamFeature migCounter row)))

type EffectStack =
  [ State Int,
    Input ClientState,
    Input Hasql.Pool,
    Async,
    Race,
    TinyLog,
    Embed IO,
    Final IO
  ]

migrateTeamFeaturesLoop ::
  MigrationOptions ->
  ClientState ->
  Hasql.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  IO ()
migrateTeamFeaturesLoop migOpts cassClient pgPool logger migCounter migFinished migFailed =
  migrationLoop
    logger
    "team features"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "team features")
    (migrateAllTeamFeatures migOpts migCounter)

interpreter :: ClientState -> Hasql.Pool -> Log.Logger -> ByteString -> Sem EffectStack a -> IO (Int, a)
interpreter cassClient pgPool logger name =
  runFinal
    . embedToFinal
    . loggerToTinyLog logger
    . mapLogger (Log.field "migration" name .)
    . raiseUnder
    . interpretRace
    . asyncToIOFinal
    . runInputConst pgPool
    . runInputConst cassClient
    . runState 0

migrateTeamFeature ::
  ( PGConstraints r,
    Member TinyLog r,
    Member Async r,
    Member (Error MigrationLockError) r,
    Member Race r
  ) =>
  Prometheus.Counter ->
  (TeamId, Text, Maybe FeatureStatus, Maybe LockStatus, Maybe DbConfig) ->
  Sem r ()
migrateTeamFeature migCounter (tid, name, status, lockStatus, dbConfig) = do
  -- We do not delete Cassandra rows during migration. Writes and migration use
  -- the same per-row lock, so we avoid races without deleting early. Deletion is
  -- deferred to keep rollback options and to remove the Cassandra table only after
  -- a full cutover to Postgres-only.
  void . withMigrationLocks LockExclusive (Seconds 10) [(tid, name)] $ do
    isMigrated <- runStatement (tid, name) Psql.exists
    unless isMigrated $ do
      runStatement (tid, name, status, lockStatus, dbConfig) Psql.upsertPatch
      liftIO $ Prometheus.incCounter migCounter

handleErrors ::
  ( Member (State Int) r,
    Member TinyLog r
  ) =>
  ByteString ->
  (Sem (Error MigrationLockError : Error Hasql.UsageError : r) ()) ->
  Sem r ()
handleErrors key action = do
  eithErr <- runError (runError action)
  case eithErr of
    Right (Right _) -> pure ()
    Right (Left e) -> do
      warn $
        Log.msg (Log.val "error occurred during migration")
          . Log.field "key" (show key)
          . Log.field "error" (show e)
      modify (+ 1)
    Left e -> do
      warn $
        Log.msg (Log.val "error occurred during migration")
          . Log.field "key" (show key)
          . Log.field "error" (show e)
      modify (+ 1)

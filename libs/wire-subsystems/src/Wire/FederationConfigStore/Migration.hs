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

module Wire.FederationConfigStore.Migration (migrateFederationConfigsLoop) where

import Cassandra hiding (Value)
import Data.ByteString.Conversion (toByteString')
import Data.Conduit
import Data.Conduit.List qualified as C
import Data.Domain (Domain, domainText)
import Hasql.Pool.Extended qualified as Hasql
import Imports
import Polysemy
import Polysemy.Async
import Polysemy.AtomicState
import Polysemy.Conc (interpretRace)
import Polysemy.Conc.Effect.Race hiding (Timeout)
import Polysemy.Error
import Polysemy.Input
import Polysemy.Resource (Resource, resourceToIOFinal)
import Polysemy.TinyLog
import Prometheus qualified
import System.Logger qualified as Log
import Wire.API.User.Search (FederatedUserSearchPolicy)
import Wire.FederationConfigStore.Cassandra qualified as Cql
import Wire.FederationConfigStore.Postgres qualified as Postgres
import Wire.Migration
import Wire.MigrationLock
import Wire.Postgres
import Wire.Sem.Logger (mapLogger)
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)

type EffectStack =
  [ AtomicState Int,
    Input ClientState,
    Input Hasql.Pool,
    Resource,
    Async,
    Race,
    TinyLog,
    Embed IO,
    Final IO
  ]

migrateFederationConfigsLoop ::
  MigrationOptions ->
  ClientState ->
  Hasql.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  IO ()
migrateFederationConfigsLoop migOpts cassClient pgPool logger migCounter migFinished migFailed migDuration =
  migrationLoop
    logger
    "federation config"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "federation config")
    (migrateAllFederationConfigs migOpts migCounter migDuration)

interpreter :: ClientState -> Hasql.Pool -> Log.Logger -> ByteString -> Sem EffectStack a -> IO (Int, a)
interpreter cassClient pgPool logger name =
  runFinal
    . embedToFinal
    . loggerToTinyLog logger
    . mapLogger (Log.field "migration" (Log.val name) .)
    . raiseUnder
    . interpretRace
    . asyncToIOFinal
    . resourceToIOFinal
    . runInputConst pgPool
    . runInputConst cassClient
    . atomicStateToIO 0

migrateAllFederationConfigs ::
  ( Member (Input Hasql.Pool) r,
    Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r,
    Member (AtomicState Int) r,
    Member Async r,
    Member Resource r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  ConduitM () Void (Sem r) ()
migrateAllFederationConfigs migOpts migCounter migDuration = do
  lift $ info $ Log.msg (Log.val "migrateAllFederationConfigs")
  withCount
    (paginateSem Cql.selectAllFederationRemotes (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| C.mapM_
      ( traverse_
          ( \row@(domain, _, _) ->
              handleLockAndDBErrors (toByteString' (domainText domain)) (migrateFederationConfigRow migOpts migCounter migDuration row)
          )
      )

migrateFederationConfigRow ::
  ( PGConstraints r,
    Member TinyLog r,
    Member Async r,
    Member (Error MigrationLockError) r,
    Member Race r,
    Member Resource r,
    Member (Input ClientState) r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  (Domain, FederatedUserSearchPolicy, Maybe Int32) ->
  Sem r ()
migrateFederationConfigRow migOpts migCounter migDuration (domain, policy, restriction) =
  withExclusiveMigrationLockAndTimeout migOpts.timeout migDuration [domain] $ do
    Postgres.upsertFederationConfig domain policy restriction
    cs <- input @ClientState
    teams <-
      embed $ runClient cs $ do
        res <- retry x1 $ query Cql.selectFederationRemoteTeams (params LocalQuorum (Identity domain))
        pure (map runIdentity res)
    traverse_ (Postgres.insertFederationRemoteTeam domain) teams
    liftIO $ Prometheus.incCounter migCounter

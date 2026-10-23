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

module Wire.CustomBackendStore.Migration
  ( migrateCustomBackendsLoop,
  )
where

import Cassandra
import Data.ByteString.Conversion
import Data.Conduit
import Data.Conduit.List qualified as C
import Data.Domain (Domain)
import Data.IORef qualified as IORef
import Data.Misc (HttpsUrl)
import Data.Text qualified as T
import Data.Time
import Hasql.Pool.Extended qualified as Hasql
import Imports
import Polysemy
import Polysemy.Async
import Polysemy.AtomicState
import Polysemy.Conc (interpretRace)
import Polysemy.Conc qualified as Conc
import Polysemy.Conc.Effect.Race hiding (Timeout)
import Polysemy.Input
import Polysemy.Resource (Resource, bracket, resourceToIOFinal)
import Polysemy.TinyLog
import Prometheus qualified
import System.Logger qualified as Log
import UnliftIO qualified
import Wire.API.CustomBackend (CustomBackend (..))
import Wire.CustomBackendStore qualified as Store
import Wire.CustomBackendStore.Postgres qualified as Postgres
import Wire.Migration
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

migrateCustomBackendsLoop ::
  MigrationOptions ->
  ClientState ->
  Hasql.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  IO ()
migrateCustomBackendsLoop migOpts cassClient pgPool logger migCounter migFinished migFailed migDuration =
  migrationLoop
    logger
    "custom backends"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "custom backends")
    (migrateAllCustomBackends migOpts migCounter migDuration)

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

selectAllCustomBackends :: PrepQuery R () (Domain, HttpsUrl, HttpsUrl)
selectAllCustomBackends =
  "SELECT domain, config_json_url, webapp_welcome_url FROM custom_backend"

migrateAllCustomBackends ::
  ( Member (Input Hasql.Pool) r,
    Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r,
    Member (AtomicState Int) r,
    Member Resource r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  ConduitM () Void (Sem r) ()
migrateAllCustomBackends migOpts migCounter migDuration = do
  lift $ info $ Log.msg (Log.val "migrateAllCustomBackends")
  withCount (paginateSem selectAllCustomBackends (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| C.mapM_ (traverse_ (\row@(dom, _, _) -> handleErrors (toByteString' dom) (migrateCustomBackendRow migOpts migCounter migDuration row)))

-- | Copy-only: upsert the row into Postgres, Cassandra rows are not deleted.
migrateCustomBackendRow ::
  ( PGConstraints r,
    Member TinyLog r,
    Member Resource r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  (Domain, HttpsUrl, HttpsUrl) ->
  Sem r ()
migrateCustomBackendRow migOpts migCounter migDuration (dom, cfgUrl, welcomeUrl) = do
  let domText = T.pack (show dom)
  outcomeRef <- liftIO $ IORef.newIORef @Text "error"
  bracket
    (liftIO getCurrentTime)
    (observeDuration migDuration outcomeRef)
    ( const $ do
        timeoutResult <-
          Conc.timeout (migOpts.timeout <$ handleTimeout) migOpts.timeout $
            Postgres.interpretCustomBackendStoreToPostgres $
              Store.setCustomBackend
                dom
                CustomBackend
                  { backendConfigJsonUrl = cfgUrl,
                    backendWebappWelcomeUrl = welcomeUrl
                  }
        case timeoutResult of
          Left timedOutAfter -> do
            markOutcome outcomeRef "timeout"
            liftIO . UnliftIO.throwIO $ MigrationTimedOut domText timedOutAfter
          Right () -> do
            markOutcome outcomeRef "success"
            liftIO $ Prometheus.incCounter migCounter
    )
  where
    handleTimeout =
      err $
        Log.msg (Log.val "custom backend migration timed out")
          . Log.field "domain" (show dom)
          . Log.field "timeout" (show migOpts.timeout)

    markOutcome ref outcome = liftIO $ IORef.writeIORef ref outcome

    observeDuration metric outcomeRef start = do
      outcome <- liftIO $ IORef.readIORef outcomeRef
      end <- liftIO getCurrentTime
      liftIO $ Prometheus.withLabel metric outcome (`Prometheus.observe` realToFrac (diffUTCTime end start))

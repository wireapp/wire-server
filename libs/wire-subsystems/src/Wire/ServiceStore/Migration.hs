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

module Wire.ServiceStore.Migration (migrateServicesLoop) where

import Cassandra hiding (Value)
import Cassandra qualified as C
import Data.ByteString.Conversion (toByteString')
import Data.Conduit
import Data.Conduit.List qualified as C
import Data.IORef qualified as IORef
import Data.Id
import Data.Misc (Fingerprint, HttpsUrl, Rsa)
import Data.Text qualified as T
import Data.Time
import Hasql.Pool.Extended qualified as Hasql
import Imports
import Polysemy
import Polysemy.Async
import Polysemy.Conc (interpretRace)
import Polysemy.Conc qualified as Conc
import Polysemy.Conc.Effect.Race hiding (Timeout)
import Polysemy.Input
import Polysemy.Resource (Resource, bracket, resourceToIOFinal)
import Polysemy.State
import Polysemy.TinyLog
import Prometheus qualified
import System.Logger qualified as Log
import UnliftIO qualified
import Wire.API.Bot.Service qualified as Bot
import Wire.API.Provider.Service (ServiceToken, newServiceRef)
import Wire.Migration
import Wire.Postgres
import Wire.Sem.Logger (mapLogger)
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)
import Wire.ServiceStore (createService)
import Wire.ServiceStore.Cassandra.Queries qualified as Cql
import Wire.ServiceStore.Postgres qualified as Postgres

type EffectStack =
  [ State Int,
    Input ClientState,
    Input Hasql.Pool,
    Resource,
    Async,
    Race,
    TinyLog,
    Embed IO,
    Final IO
  ]

migrateServicesLoop ::
  MigrationOptions ->
  ClientState ->
  Hasql.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  IO ()
migrateServicesLoop migOpts cassClient pgPool logger migCounter migFinished migFailed migDuration =
  migrationLoop
    logger
    "services"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "services")
    (migrateAllServices migOpts migCounter migDuration)

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
    . runState 0

migrateAllServices ::
  ( Member (Input Hasql.Pool) r,
    Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r,
    Member (State Int) r,
    Member Resource r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  ConduitM () Void (Sem r) ()
migrateAllServices migOpts migCounter migDuration = do
  lift $ info $ Log.msg (Log.val "migrateAllServices")
  withCount (paginateSem Cql.selectAllServices (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| C.mapM_ (traverse_ (\row@(pid, sid, _, _, _, _) -> handleErrors (toByteString' pid <> toByteString' sid) (migrateServiceRow migOpts migCounter migDuration row)))

migrateServiceRow ::
  ( PGConstraints r,
    Member TinyLog r,
    Member Resource r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  (ProviderId, ServiceId, HttpsUrl, ServiceToken, C.Set (Fingerprint Rsa), Bool) ->
  Sem r ()
migrateServiceRow migOpts migCounter migDuration (pid, sid, url, tok, Set fps, ena) = do
  let service = Bot.Service (newServiceRef sid pid) url tok fps ena
      keyText = T.pack (show (pid, sid))
  outcomeRef <- liftIO $ IORef.newIORef @Text "error"
  bracket
    (liftIO getCurrentTime)
    (observeDuration migDuration outcomeRef)
    ( const $ do
        timeoutResult <- Conc.timeout (migOpts.timeout <$ handleTimeout) migOpts.timeout $ Postgres.interpretServiceStoreToPostgres $ createService service
        case timeoutResult of
          Left timedOutAfter -> do
            markOutcome outcomeRef "timeout"
            liftIO . UnliftIO.throwIO $ MigrationTimedOut keyText timedOutAfter
          Right () -> do
            markOutcome outcomeRef "success"
            liftIO $ Prometheus.incCounter migCounter
    )
  where
    handleTimeout =
      err $
        Log.msg (Log.val "service migration timed out")
          . Log.field "provider" (show pid)
          . Log.field "service" (show sid)
          . Log.field "timeout" (show migOpts.timeout)

    markOutcome ref outcome = liftIO $ IORef.writeIORef ref outcome

    observeDuration metric outcomeRef start = do
      outcome <- liftIO $ IORef.readIORef outcomeRef
      end <- liftIO getCurrentTime
      liftIO $ Prometheus.withLabel metric outcome (`Prometheus.observe` realToFrac (diffUTCTime end start))

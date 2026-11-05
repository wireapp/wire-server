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

module Wire.TeamNotificationStore.Migration (migrateTeamNotificationsLoop) where

import Cassandra hiding (Value)
import Data.Aeson qualified as JSON
import Data.Conduit
import Data.Conduit.List qualified as C
import Data.IORef qualified as IORef
import Data.Id
import Data.List.NonEmpty (NonEmpty)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
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
import Wire.Migration
import Wire.Postgres
import Wire.Sem.Logger (mapLogger)
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)
import Wire.TeamNotificationStore
import Wire.TeamNotificationStore.Postgres qualified as Postgres

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

migrateTeamNotificationsLoop ::
  MigrationOptions ->
  ClientState ->
  Hasql.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  IO ()
migrateTeamNotificationsLoop migOpts cassClient pgPool logger migCounter migFinished migFailed migDuration =
  migrationLoop
    logger
    "team notifications"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "team notifications")
    (migrateAllTeamNotifications migOpts migCounter migDuration)

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

-- | Full scan of the Cassandra team_notifications table, defined
-- module-locally (this store has no Queries module).
selectAllTeamNotifications :: PrepQuery R () (TeamId, TimeUuid, Blob)
selectAllTeamNotifications = "SELECT team, id, payload FROM team_notifications"

migrateAllTeamNotifications ::
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
migrateAllTeamNotifications migOpts migCounter migDuration = do
  lift $ info $ Log.msg (Log.val "migrateAllTeamNotifications")
  withCount (paginateSem selectAllTeamNotifications (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| C.mapM_
      ( traverse_ $ \row@(tid, nid, _) ->
          handleErrors (TE.encodeUtf8 (rowKeyText tid nid)) $
            migrateTeamNotificationRow migOpts migCounter migDuration row
      )

-- | Log/display key for a row, shared by the error handler and the
-- timeout/error messages so they stay correlated.
rowKeyText :: TeamId -> TimeUuid -> Text
rowKeyText tid nid = T.pack (show (toUUID tid, fromTimeUuid nid))

migrateTeamNotificationRow ::
  ( Member TinyLog r,
    Member Resource r,
    Member Race r,
    PGConstraints r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  (TeamId, TimeUuid, Blob) ->
  Sem r ()
migrateTeamNotificationRow migOpts migCounter migDuration (tid, nid, blob) =
  -- Malformed payloads are silently skipped by the Cassandra reader
  -- (toNotif); mirror that here without counting the row as a migration
  -- failure.
  case JSON.decode' (fromBlob blob) :: Maybe (NonEmpty JSON.Object) of
    Nothing ->
      warn $
        Log.msg (Log.val "skipping team notification with malformed payload")
          . Log.field "team" (show tid)
          . Log.field "id" (show nid)
    Just payload -> do
      let rowText = rowKeyText tid nid
      outcomeRef <- liftIO $ IORef.newIORef @Text "error"
      bracket
        (liftIO getCurrentTime)
        (observeDuration migDuration outcomeRef)
        ( const $ do
            timeoutResult <-
              Conc.timeout
                (migOpts.timeout <$ handleTimeout rowText)
                migOpts.timeout
                $ Postgres.interpretTeamNotificationStoreToPostgres
                $ createTeamNotification tid (Id (fromTimeUuid nid)) payload
            case timeoutResult of
              Left timedOutAfter -> do
                markOutcome outcomeRef "timeout"
                liftIO . UnliftIO.throwIO $ MigrationTimedOut rowText timedOutAfter
              Right () -> do
                markOutcome outcomeRef "success"
                liftIO $ Prometheus.incCounter migCounter
        )
  where
    handleTimeout rowText =
      err $
        Log.msg (Log.val "team notification migration timed out")
          . Log.field "row" rowText
          . Log.field "timeout" (show migOpts.timeout)

    markOutcome ref outcome = liftIO $ IORef.writeIORef ref outcome

    observeDuration metric outcomeRef start = do
      outcome <- liftIO $ IORef.readIORef outcomeRef
      end <- liftIO getCurrentTime
      liftIO $ Prometheus.withLabel metric outcome (`Prometheus.observe` realToFrac (diffUTCTime end start))

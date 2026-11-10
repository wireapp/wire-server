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

module Wire.NotificationStore.Migration (migrateNotificationsLoop) where

import Cassandra hiding (Value)
import Cassandra qualified as Cql
import Data.ByteString.Lazy qualified as LBS
import Data.Conduit
import Data.Conduit.List qualified as C
import Data.IORef qualified as IORef
import Data.Id
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time
import Data.UUID (UUID)
import Hasql.Pool.Extended qualified as Hasql
import Imports
import Polysemy
import Polysemy.Async
import Polysemy.AtomicState
import Polysemy.Conc (interpretRace)
import Polysemy.Conc qualified as Conc
import Polysemy.Conc.Effect.Race hiding (Timeout)
import Polysemy.Input
import Polysemy.Error (Error)
import Polysemy.Resource (Resource, bracket, resourceToIOFinal)
import Polysemy.TinyLog
import Prometheus qualified
import System.Logger qualified as Log
import UnliftIO qualified
import Wire.Migration
import Wire.MigrationLock (MigrationLockError)
import Wire.NotificationStore.Postgres qualified as Postgres
import Wire.Postgres
import Wire.Sem.Logger (mapLogger)
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)
import Wire.Sem.Concurrency
import Wire.Sem.Concurrency.IO (unsafelyPerformConcurrency)
import Wire.Util (embedClient)

type EffectStack =
  [ AtomicState Int,
    Input ClientState,
    Input Hasql.Pool,
    Resource,
    Async,
    Race,
    TinyLog,
    Embed IO,
    Concurrency 'Unsafe,
    Final IO
  ]

migrateNotificationsLoop ::
  MigrationOptions ->
  ClientState ->
  Hasql.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  IO ()
migrateNotificationsLoop migOpts cassClient pgPool logger migCounter migFinished migFailed migDuration =
  migrationLoop
    logger
    "notifications"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "notifications")
    (migrateAllNotifications migOpts migCounter migDuration)

interpreter :: ClientState -> Hasql.Pool -> Log.Logger -> ByteString -> Sem EffectStack a -> IO (Int, a)
interpreter cassClient pgPool logger name =
  runFinal
    . unsafelyPerformConcurrency
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

-- | Full scan of the Cassandra notification_payload table, defined
-- module-locally (this store has no Queries module).
selectAllPayloads :: PrepQuery R () (UUID, Blob, Maybe Int32)
selectAllPayloads = "SELECT id, payload, ttl(payload) FROM notification_payload"

-- | Full scan of the Cassandra notifications table. Rows are written with a
-- single USING TTL on the payload (or payload_ref) column, so the two ttl(...)
-- readings agree; rows where both are Nothing expired mid-scan and are
-- skipped.
selectAllNotifications :: PrepQuery R () (UUID, TimeUuid, Maybe Blob, Maybe UUID, Maybe Int32, Cql.Set ClientId, Maybe Int32, Maybe Int32)
selectAllNotifications =
  "SELECT user, id, payload, payload_ref, payload_ref_size, clients, ttl(payload), ttl(payload_ref) \
  \FROM notifications"

migrateAllNotifications ::
  ( Member (Input Hasql.Pool) r,
    Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r,
    Member (AtomicState Int) r,
    Member Resource r,
    Member Race r,
    Member Async r,
    Member (Concurrency 'Unsafe) r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  ConduitM () Void (Sem r) ()
migrateAllNotifications migOpts migCounter migDuration = do
  lift $ info $ Log.msg (Log.val "migrateAllNotifications: notification_payload")
  withCount (paginateSem selectAllPayloads (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| C.mapM_
      ( unsafePooledMapConcurrentlyN_ migOpts.parallelism $ \row@(payloadId, _, _) ->
          handleErrors (TE.encodeUtf8 (rowKeyText payloadId payloadId)) $
            migrateNotificationPayloadRow migOpts migCounter migDuration row
      )
  lift $ info $ Log.msg (Log.val "migrateAllNotifications: notifications")
  withCount (paginateSem selectAllNotifications (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| C.mapM_
      ( unsafePooledMapConcurrentlyN_ migOpts.parallelism $ \row@(userId, nid, _, _, _, _, _, _) ->
          handleLockAndDBErrors (TE.encodeUtf8 (rowKeyText userId nid)) $
            migrateNotificationRow migOpts migCounter migDuration row
      )

-- | Log/display key for a row, shared by the error handler and the
-- timeout/error messages so they stay correlated.
rowKeyText :: (Show a, Show b) => a -> b -> Text
rowKeyText uid nid = T.pack (show (uid, nid))

-- | Copy one @notification_payload@ row. Payload ids are RFC-4122 v4, so
-- @occurred_at@ anchors at the copy time; rows whose TTL already expired
-- mid-scan are skipped.
migrateNotificationPayloadRow ::
  ( Member TinyLog r,
    Member Resource r,
    Member Race r,
    PGConstraints r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  (UUID, Blob, Maybe Int32) ->
  Sem r ()
migrateNotificationPayloadRow migOpts migCounter migDuration (payloadId, blob, mbTtl) =
  case mbTtl of
    Nothing ->
      pure ()
    Just remaining -> do
      now <- embed getCurrentTime
      let payload = LBS.toStrict (fromBlob blob)
          occurred = fromMaybe now (Postgres.notificationIdTime (Id payloadId))
          expires = addUTCTime (fromIntegral remaining) now
      withTimeoutAndMetrics migOpts migCounter migDuration (rowKeyText payloadId payloadId) $
        Postgres.insertNotificationPayloadRow (Id payloadId) payload occurred expires

-- | Copy one @notifications@ row. Payload bytes are copied raw (no
-- re-encode); @occurred_at@ is decoded from the RFC 4122 v1 timestamp of the
-- notification id (falling back to the copy time for non-v1 ids).
--
-- The row is copied under the exclusive per-user migration lock, and the
-- row is re-read from Cassandra inside the lock: rows enumerated by the
-- full scan may have been deleted meanwhile (e.g. 'DeleteAllNotifications'
-- during account removal). Without the lock and re-read, a stale row would
-- be resurrected in Postgres and nothing would self-heal (the Cassandra
-- partition is cleared, so a later copy pass never sees the delete again).
migrateNotificationRow ::
  ( Member TinyLog r,
    Member Resource r,
    Member Race r,
    Member Async r,
    Member (Input ClientState) r,
    PGConstraints r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  (UUID, TimeUuid, Maybe Blob, Maybe UUID, Maybe Int32, Cql.Set ClientId, Maybe Int32, Maybe Int32) ->
  Sem (Error MigrationLockError : r) ()
migrateNotificationRow migOpts migCounter migDuration (userId, nid, _, _, _, _, _, _) =
  withExclusiveMigrationLockAndTimeout migOpts.timeout migDuration [Id userId :: UserId] $ do
    client <- input @ClientState
    mRow <-
      embedClient client $
        retry x1 $
          query1
            selectNotificationForCopy
            (params LocalQuorum (userId, nid))
    case mRow of
      -- deleted after enumeration; do not resurrect it
      Nothing -> pure ()
      Just (mbPayload, mbPayloadRef, mbPayloadRefSize, clients, mbPayloadTtl, mbRefTtl) ->
        case mbPayloadTtl <|> mbRefTtl of
          Nothing -> pure ()
          Just remaining -> do
            now <- embed getCurrentTime
            let occurred = fromMaybe now (Postgres.notificationIdTime (Id (fromTimeUuid nid)))
                expires = addUTCTime (fromIntegral remaining) now
            Postgres.insertNotificationRow
              (Id userId)
              (Id (fromTimeUuid nid))
              (fmap (LBS.toStrict . fromBlob) mbPayload)
              (Id <$> mbPayloadRef)
              mbPayloadRefSize
              (Set.fromList (Cql.fromSet clients))
              occurred
              expires
            liftIO $ Prometheus.incCounter migCounter

-- | Point re-read used inside the exclusive lock to detect rows that were
-- deleted after the full scan enumerated them.
selectNotificationForCopy ::
  PrepQuery R (UUID, TimeUuid) (Maybe Blob, Maybe UUID, Maybe Int32, Cql.Set ClientId, Maybe Int32, Maybe Int32)
selectNotificationForCopy =
  "SELECT payload, payload_ref, payload_ref_size, clients, ttl(payload), ttl(payload_ref) \
  \FROM notifications WHERE user = ? AND id = ?"

-- | Per-row bracket + timeout + duration histogram, mirroring the other
-- store migrations.
withTimeoutAndMetrics ::
  ( Member TinyLog r,
    Member Resource r,
    Member Race r,
    Member (Embed IO) r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  Text ->
  Sem r () ->
  Sem r ()
withTimeoutAndMetrics migOpts migCounter migDuration rowText action = do
  outcomeRef <- liftIO $ IORef.newIORef @Text "error"
  bracket
    (liftIO getCurrentTime)
    (observeDuration migDuration outcomeRef)
    ( const $ do
        timeoutResult <-
          Conc.timeout
            (migOpts.timeout <$ handleTimeout rowText)
            migOpts.timeout
            action
        case timeoutResult of
          Left timedOutAfter -> do
            markOutcome outcomeRef "timeout"
            liftIO . UnliftIO.throwIO $ MigrationTimedOut rowText timedOutAfter
          Right () -> do
            markOutcome outcomeRef "success"
            liftIO $ Prometheus.incCounter migCounter
    )
  where
    handleTimeout row =
      err $
        Log.msg (Log.val "notification migration timed out")
          . Log.field "row" row
          . Log.field "timeout" (show migOpts.timeout)

    markOutcome ref outcome = liftIO $ IORef.writeIORef ref outcome

    observeDuration metric outcomeRef start = do
      outcome <- liftIO $ IORef.readIORef outcomeRef
      end <- liftIO getCurrentTime
      liftIO $ Prometheus.withLabel metric outcome (`Prometheus.observe` realToFrac (diffUTCTime end start))

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

module Wire.ScimUserMetaStore.Migration (migrateScimUserMetaLoop) where

import Cassandra hiding (Value)
import Data.Conduit
import Data.Conduit.List qualified as C
import Data.IORef qualified as IORef
import Data.Id (Id (..))
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Data.UUID (UUID)
import Data.UUID qualified as UUID
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
import Wire.API.PostgresMarshall
import Wire.Migration
import Wire.Postgres
import Wire.ScimUserMetaStore.Postgres qualified as Postgres
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

migrateScimUserMetaLoop ::
  MigrationOptions ->
  ClientState ->
  Hasql.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  IO ()
migrateScimUserMetaLoop migOpts cassClient pgPool logger migCounter migFinished migFailed migDuration =
  migrationLoop
    logger
    "scim user meta"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "scim user meta")
    (migrateAllScimUserMeta migOpts migCounter migDuration)

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

selectAllScimUserTimes :: PrepQuery R () (UUID, UTCTime, UTCTime, Maybe Text, Maybe Bool)
selectAllScimUserTimes =
  "SELECT uid, created_at, last_updated_at, email_type, email_primary FROM scim_user_times"

migrateAllScimUserMeta ::
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
migrateAllScimUserMeta migOpts migCounter migDuration = do
  lift $ info $ Log.msg (Log.val "migrateAllScimUserMeta")
  withCount (paginateSem selectAllScimUserTimes (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| C.mapM_ (traverse_ (\row@(uid, _, _, _, _) -> handleErrors (encodeUtf8 (UUID.toText uid)) (migrateScimUserMetaRow migOpts migCounter migDuration row)))

migrateScimUserMetaRow ::
  ( PGConstraints r,
    Member TinyLog r,
    Member Resource r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  (UUID, UTCTime, UTCTime, Maybe Text, Maybe Bool) ->
  Sem r ()
migrateScimUserMetaRow migOpts migCounter migDuration (uid, createdAt, lastUpdatedAt, emailType, emailPrimary) =
  -- Rows that fail to decode indicate corruption; skip them rather than fail
  -- the migration (same convention as the Postgres read path).
  for_ (hush (postgresUnmarshall createdAt) >>= \c -> hush (postgresUnmarshall lastUpdatedAt) >>= \l -> pure (c, l)) $ \(createdAt', lastUpdatedAt') -> do
    let uid' = Id uid
    outcomeRef <- liftIO $ IORef.newIORef @Text "error"
    bracket
      (liftIO getCurrentTime)
      (observeDuration migDuration outcomeRef)
      ( const $ do
          timeoutResult <-
            Conc.timeout (migOpts.timeout <$ handleTimeout) migOpts.timeout $
              Postgres.upsertScimUserMetaRow uid' createdAt' lastUpdatedAt' emailType emailPrimary
          case timeoutResult of
            Left timedOutAfter -> do
              markOutcome outcomeRef "timeout"
              liftIO . UnliftIO.throwIO $ MigrationTimedOut (UUID.toText uid) timedOutAfter
            Right () -> do
              markOutcome outcomeRef "success"
              liftIO $ Prometheus.incCounter migCounter
      )
  where
    handleTimeout =
      err $
        Log.msg (Log.val "scim user meta migration timed out")
          . Log.field "key" (UUID.toText uid)
          . Log.field "timeout" (show migOpts.timeout)

    markOutcome ref outcome = liftIO $ IORef.writeIORef ref outcome

    observeDuration metric outcomeRef start = do
      outcome <- liftIO $ IORef.readIORef outcomeRef
      end <- liftIO getCurrentTime
      liftIO $ Prometheus.withLabel metric outcome (`Prometheus.observe` realToFrac (diffUTCTime end start))

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

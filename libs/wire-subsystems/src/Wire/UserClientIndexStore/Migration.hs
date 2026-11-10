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

-- | Background copy loop: full Cassandra scan of the galley @clients@ table
-- (the @clients@ set column) mirrored row-by-row into the Postgres
-- @user_client_index@ table.
--
-- Copy-only: Cassandra rows are never deleted. Re-runs are idempotent
-- (@ON CONFLICT DO NOTHING@).
--
-- Accepted race (5414 service-store precedent): a client deleted by dual-write
-- traffic while a scan page is in flight can leave a stale @user_client_index@
-- row — the page was fetched before the delete, the mirror delete removed the
-- row from both stores, and the page's @INSERT ... ON CONFLICT DO NOTHING@
-- re-inserts it afterwards (the row was absent, so DO NOTHING does not fire).
-- There is no deletion-marker table because the Cassandra source (the
-- @clients@ set column) carries no per-entry tombstones a full scan could
-- read. The window is bounded by the scan duration and invisible while
-- Cassandra is the source of truth (reads route to Cassandra in
-- migration-to-postgresql mode); a stale row is a dangling index entry for a
-- client that no longer exists.
--
-- Reconciliation at cutover: brig's @clients@ table (the client-store
-- migration, WPB-29574) is the authoritative client inventory once brig is cut
-- over. Operators can rebuild the index from it in one statement if a stale
-- row is suspected:
--
-- > DELETE FROM user_client_index;
-- > INSERT INTO user_client_index SELECT user_id, client_id FROM clients;
module Wire.UserClientIndexStore.Migration
  ( migrateUserClientIndexLoop,
  )
where

import Cassandra
import Data.ByteString.Conversion (toByteString')
import Data.Conduit
import Data.Conduit.List qualified as C
import Data.IORef qualified as IORef
import Data.Id
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
import Wire.Migration
import Wire.Postgres
import Wire.Sem.Logger (mapLogger)
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)
import Wire.UserClientIndexStore
import Wire.UserClientIndexStore.Postgres qualified as Postgres

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

migrateUserClientIndexLoop ::
  MigrationOptions ->
  ClientState ->
  Hasql.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  IO ()
migrateUserClientIndexLoop migOpts cassClient pgPool logger migCounter migFinished migFailed migDuration =
  migrationLoop
    logger
    "user client index"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "user client index")
    (migrateAllUserClients migOpts migCounter migDuration)

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

selectAllClients :: PrepQuery R () (UserId, Cassandra.Set ClientId)
selectAllClients =
  "SELECT user, clients FROM clients"

migrateAllUserClients ::
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
migrateAllUserClients migOpts migCounter migDuration = do
  lift $ info $ Log.msg (Log.val "migrateAllUserClients")
  withCount (paginateSem selectAllClients (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| C.mapM_
      ( traverse_ $ \row@(u, _) ->
          handleErrors
            (toByteString' u)
            (migrateRow migOpts migCounter migDuration row)
      )

-- | Copy-only row insert into Postgres; Cassandra rows are not deleted.
migrateRow ::
  ( PGConstraints r,
    Member TinyLog r,
    Member Resource r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  (UserId, Cassandra.Set ClientId) ->
  Sem r ()
migrateRow migOpts migCounter migDuration (u, cs) = do
  outcomeRef <- liftIO $ IORef.newIORef @Text "error"
  bracket
    (liftIO getCurrentTime)
    (observeDuration migDuration outcomeRef)
    ( const $ do
        timeoutResult <-
          Conc.timeout (migOpts.timeout <$ handleTimeout) migOpts.timeout $ do
            forM_ (Cassandra.fromSet cs) $ \cid ->
              Postgres.interpretUserClientIndexStoreToPostgres $ createClient u cid
            liftIO $ Prometheus.incCounter migCounter
        case timeoutResult of
          Left timedOutAfter -> do
            markOutcome outcomeRef "timeout"
            liftIO . UnliftIO.throwIO $ MigrationTimedOut (tshow u) timedOutAfter
          Right () -> markOutcome outcomeRef "success"
    )
  where
    tshow = T.pack . show

    handleTimeout =
      err $
        Log.msg (Log.val "user client index row migration timed out")
          . Log.field "user" (show u)
          . Log.field "timeout" (show migOpts.timeout)

    markOutcome ref outcome = liftIO $ IORef.writeIORef ref outcome

    observeDuration metric outcomeRef start = do
      outcome <- liftIO $ IORef.readIORef outcomeRef
      end <- liftIO getCurrentTime
      liftIO $ Prometheus.withLabel metric outcome (`Prometheus.observe` realToFrac (diffUTCTime end start))

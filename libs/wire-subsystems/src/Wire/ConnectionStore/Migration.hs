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

module Wire.ConnectionStore.Migration
  ( migrateConnectionsLoop,
  )
where

import Cassandra
import Data.Conduit
import Data.Conduit.List qualified as C
import Data.Domain (Domain)
import Data.Id
import Data.Json.Util
import Data.Time
import Hasql.Pool qualified as Hasql
import Hasql.Pool.Extended qualified as HasqlPoolExt
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Polysemy.Async
import Polysemy.AtomicState
import Polysemy.Conc (interpretRace)
import Polysemy.Conc.Effect.Race hiding (Timeout)
import Polysemy.Error (Error, runError)
import Polysemy.Input
import Polysemy.Resource (Resource, resourceToIOFinal)
import Polysemy.TinyLog
import Prometheus qualified
import System.Logger qualified as Log
import Wire.API.Connection (RelationWithHistory)
import Wire.API.PostgresMarshall (PostgresMarshall (..), lmapPG)
import Wire.ConnectionStore.Cassandra
  ( connectionsSelect,
    remoteConnectionSelect,
  )
import Wire.Migration
import Wire.Postgres
import Wire.Sem.Logger (mapLogger)
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)

type EffectStack =
  [ AtomicState Int,
    Input ClientState,
    Input HasqlPoolExt.Pool,
    Resource,
    Async,
    Race,
    TinyLog,
    Embed IO,
    Final IO
  ]

-- | Copy-only migration of the @connection@ and @connection_remote@ tables
-- from Cassandra to Postgres. Two sequential sub-loops share one
-- @finished@ counter; the loop reports 1.0 once both tables are copied.
migrateConnectionsLoop ::
  MigrationOptions ->
  ClientState ->
  HasqlPoolExt.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  IO ()
migrateConnectionsLoop migOpts cassClient pgPool logger migCounter migFinished migFailed migDuration =
  migrationLoop
    logger
    "connections"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "connections")
    (migrateAllConnections migOpts migCounter migFailed migDuration)

interpreter :: ClientState -> HasqlPoolExt.Pool -> Log.Logger -> ByteString -> Sem EffectStack a -> IO (Int, a)
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

migrateAllConnections ::
  ( Member (Input HasqlPoolExt.Pool) r,
    Member (Input ClientState) r,
    Member (Embed IO) r,
    Member TinyLog r,
    Member (AtomicState Int) r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  ConduitM () Void (Sem r) ()
migrateAllConnections migOpts migCounter migFailed migDuration = do
  lift $ info $ Log.msg (Log.val "migrating connection table")
  copyTable migOpts migCounter migFailed migDuration selectConnectionLefts connectionsSelect upsertConnection
  lift $ info $ Log.msg (Log.val "migrating connection_remote table")
  copyTable migOpts migCounter migFailed migDuration selectRemoteConnectionLefts remoteConnectionSelect upsertRemoteConnection

-- | For every partition key (left user) page all rows out of Cassandra and
-- upsert them into Postgres, preserving the original @last_update@.
copyTable ::
  forall row r.
  ( Member (Input HasqlPoolExt.Pool) r,
    Member (Input ClientState) r,
    Member (Embed IO) r,
    Member TinyLog r,
    Member (AtomicState Int) r,
    Tuple row
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  PrepQuery R () (Identity UserId) ->
  PrepQuery R (Identity UserId) row ->
  ( forall r2.
    (Member (Input HasqlPoolExt.Pool) r2, Member (Embed IO) r2) =>
    UserId -> row -> Sem (Error Hasql.UsageError ': r2) ()
  ) ->
  ConduitM () Void (Sem r) ()
copyTable migOpts migCounter migFailed migDuration selectLefts selectRows upsertRow =
  withCount (paginateSem selectLefts (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize runIdentity
    .| C.mapM_ (traverse_ copyPartition)
  where
    copyPartition :: UserId -> Sem r ()
    copyPartition left =
      runConduit $
        paginateSem selectRows (paramsP LocalQuorum (Identity left) migOpts.pageSize) x1
          .| C.mapM_ (traverse_ (migrateRow left migCounter migFailed migDuration . upsertRow left))

migrateRow ::
  ( Member (Embed IO) r,
    Member TinyLog r,
    Member (AtomicState Int) r
  ) =>
  UserId ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  Sem (Error Hasql.UsageError ': r) () ->
  Sem r ()
migrateRow left migCounter _migFailed migDuration action = do
  start <- embed @IO getCurrentTime
  result <- runError action
  end <- embed @IO getCurrentTime
  let outcome = either (const "error") (const "success") result
  liftIO $
    Prometheus.withLabel migDuration outcome (`Prometheus.observe` realToFrac (diffUTCTime end start))
  case result of
    Right () ->
      liftIO $ Prometheus.incCounter migCounter
    Left failure -> do
      warn $
        Log.msg (Log.val "connection row migration failed")
          . Log.field "left" (show left)
          . Log.field "error" (show failure)
      -- NOTE: unlike the Prometheus counter, the AtomicState count is what
      -- makes migrationLoop retry the failed rows; the counter is reserved for
      -- fatal errors (see Wire.Migration.migrationLoop).
      atomicModify (+ 1)

selectConnectionLefts :: PrepQuery R () (Identity UserId)
selectConnectionLefts = "SELECT DISTINCT left FROM connection"

selectRemoteConnectionLefts :: PrepQuery R () (Identity UserId)
selectRemoteConnectionLefts = "SELECT DISTINCT left FROM connection_remote"

-- Upserts preserving the original Cassandra timestamps.

upsertConnection ::
  ( Member (Input HasqlPoolExt.Pool) r,
    Member (Embed IO) r
  ) =>
  UserId ->
  (UserId, UserId, RelationWithHistory, UTCTimeMillis, Maybe ConvId) ->
  Sem (Error Hasql.UsageError ': r) ()
upsertConnection _left (l, r, rel, ts, mconv) =
  runStatement (l, r, postgresMarshall rel, fromUTCTimeMillis ts, mconv) stmt
  where
    stmt :: Hasql.Statement (UserId, UserId, Int32, UTCTime, Maybe ConvId) ()
    stmt =
      lmapPG
        [resultlessStatement|INSERT INTO connection ("left", "right", status, last_update, conv)
                           VALUES ($1 :: uuid, $2 :: uuid, $3 :: int4, $4 :: timestamptz, $5 :: uuid?)
                           ON CONFLICT ("left", "right") DO UPDATE
                           SET status = ($3 :: int4), last_update = ($4 :: timestamptz), conv = ($5 :: uuid?)
                          |]

upsertRemoteConnection ::
  ( Member (Input HasqlPoolExt.Pool) r,
    Member (Embed IO) r
  ) =>
  UserId ->
  (Domain, UserId, RelationWithHistory, UTCTimeMillis, Domain, ConvId) ->
  Sem (Error Hasql.UsageError ': r) ()
upsertRemoteConnection left (domain, r, rel, ts, convDomain, convId) =
  runStatement (left, domain, r, postgresMarshall rel, fromUTCTimeMillis ts, convDomain, convId) stmt
  where
    stmt :: Hasql.Statement (UserId, Domain, UserId, Int32, UTCTime, Domain, ConvId) ()
    stmt =
      lmapPG
        [resultlessStatement|INSERT INTO connection_remote ("left", right_domain, right_user, status, last_update, conv_domain, conv_id)
                           VALUES ($1 :: uuid, $2 :: text, $3 :: uuid, $4 :: int4, $5 :: timestamptz, $6 :: text, $7 :: uuid)
                           ON CONFLICT ("left", right_domain, right_user) DO UPDATE
                           SET status = ($4 :: int4), last_update = ($5 :: timestamptz), conv_domain = ($6 :: text), conv_id = ($7 :: uuid)
                          |]

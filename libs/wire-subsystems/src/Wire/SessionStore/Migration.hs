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

module Wire.SessionStore.Migration (migrateSessionsLoop) where

import Cassandra hiding (Value)
import Data.Conduit
import Data.Conduit.List qualified as C
import Data.Id
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Hasql.Pool (UsageError)
import Hasql.Pool.Extended qualified as Hasql
import Imports
import Polysemy
import Polysemy.Async
import Polysemy.AtomicState
import Polysemy.Conc (interpretRace)
import Polysemy.Conc.Effect.Race hiding (Timeout)
import Polysemy.Error (Error)
import Polysemy.Input
import Polysemy.Resource (Resource, resourceToIOFinal)
import Polysemy.TinyLog
import Prometheus qualified
import System.Logger qualified as Log
import Wire.API.User.Auth
import Wire.Migration
import Wire.MigrationLock (MigrationLockError)
import Wire.Sem.Logger (mapLogger)
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)
import Wire.SessionStore.Cassandra qualified as Cql
import Wire.SessionStore.Postgres qualified as Postgres
import Wire.Util (embedClientInput)

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

migrateSessionsLoop ::
  MigrationOptions ->
  ClientState ->
  Hasql.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  IO ()
migrateSessionsLoop migOpts cassClient pgPool logger migCounter migFinished migFailed migDuration =
  migrationLoop
    logger
    "sessions"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "sessions")
    (migrateAllSessions migOpts migCounter migDuration)

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

migrateAllSessions ::
  ( Member (Input Hasql.Pool) r,
    Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r,
    Member (AtomicState Int) r,
    Member Resource r,
    Member Async r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  ConduitM () Void (Sem r) ()
migrateAllSessions migOpts migCounter migDuration = do
  lift $ info $ Log.msg (Log.val "migrateAllSessions")
  withCount
    (paginateSem Cql.selectAllSessions (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    -- Stamp each row with an absolute validity deadline computed from its
    -- remaining Cassandra TTL at page-fetch time.  Applying the TTL relative
    -- to the insert time instead would restart the TTL for rows queued behind
    -- others and extend their validity.
    .| C.mapM
      ( \rows -> do
          now <- liftIO getCurrentTime
          pure
            [ (row, fmap (\t -> addUTCTime (fromIntegral t) now) mttl)
            | row@(_, _, _, _, _, _, _, mttl) <- rows
            ]
      )
    .| C.mapM_
      ( traverse_
          ( \item@(row, _) ->
              handleLockAndDBErrors
                (encodeUtf8 (rowKey row))
                (migrateSessionRow migOpts migCounter migDuration item)
          )
      )

rowKey :: (UserId, CookieId, UTCTime, UTCTime, CookieType, Maybe CookieLabel, Maybe CookieId, Maybe Int32) -> Text
rowKey (uid, cid, _, _, _, _, _, _) =
  Text.pack (show (uid, cookieIdNum cid))

migrateSessionRow ::
  ( Member (Input Hasql.Pool) r,
    Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r,
    Member Async r,
    Member Race r,
    Member Resource r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  ((UserId, CookieId, UTCTime, UTCTime, CookieType, Maybe CookieLabel, Maybe CookieId, Maybe Int32), Maybe UTCTime) ->
  Sem (Error MigrationLockError : Error UsageError : r) ()
migrateSessionRow migOpts migCounter migDuration ((uid, cid, created, expires, typ, label, csucc, _), mdeadline) = do
  let cookie =
        Cookie
          { cookieId = cid,
            cookieType = typ,
            cookieCreated = created,
            cookieExpires = expires,
            cookieLabel = label,
            cookieSucc = csucc,
            cookieValue = ()
          }
  -- Carry the row's remaining Cassandra TTL over into the Postgres
  -- @invalid_after@ boundary as an absolute deadline computed at scan time;
  -- inserting with the full @expires_at@ would resurrect sessions Cassandra
  -- is about to expire.  Rows with a missing or exhausted TTL are skipped.
  now <- liftIO getCurrentTime
  case mdeadline of
    Just deadline | deadline > now -> do
      withExclusiveMigrationLockAndTimeout migOpts.timeout migDuration [uid] $ do
        -- Re-read the row inside the exclusive lock: dual-write mutations hold
        -- a shared lock, so a cookie revoked after the scan page was fetched
        -- either is gone here (skip) or was already deleted from Postgres.
        alive <- embedClientInput (Cql.sessionRowExists uid expires cid)
        when alive $ do
          Postgres.insertCookieWithDeadline uid cookie deadline
          liftIO $ Prometheus.incCounter migCounter
    _ -> pure ()

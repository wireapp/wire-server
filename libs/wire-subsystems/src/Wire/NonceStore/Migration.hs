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

module Wire.NonceStore.Migration (migrateNonceLoop) where

import Cassandra hiding (Value)
import Data.ByteString.Conversion (toByteString')
import Data.Conduit
import Data.Conduit.List qualified as C
import Data.IORef qualified as IORef
import Data.Id (UserId)
import Data.Nonce (Nonce (..), NonceTtlSecs (..))
import Data.Text qualified as Text
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
import Wire.NonceStore.Cassandra qualified as Cql
import Wire.NonceStore.Postgres qualified as Postgres
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

migrateNonceLoop ::
  MigrationOptions ->
  ClientState ->
  Hasql.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  IO ()
migrateNonceLoop migOpts cassClient pgPool logger migCounter migFinished migFailed migDuration =
  migrationLoop
    logger
    "nonce"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "nonce")
    (migrateAllNonces migOpts migCounter migDuration)

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

migrateAllNonces ::
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
migrateAllNonces migOpts migCounter migDuration = do
  lift $ info $ Log.msg (Log.val "migrateAllNonces")
  withCount
    (paginateSem Cql.selectAllNonces (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| C.mapM_
      ( traverse_
          ( \(uid, key, nonce, ttl) ->
              handleErrors (toByteString' uid <> toByteString' key) (migrateNonceRow migOpts migCounter migDuration (uid, key, nonce) ttl)
          )
      )

-- | Copy-only: nonces with a lapsed or absent TTL are expired and skipped.
--
-- Note: an upsert raced by a dual-write consume can briefly resurrect a
-- consumed nonce (re-inserting the row the consume deleted). The window is one
-- copy pass, capped by the 5-minute TTL, and self-heals on the next dual-write
-- consume for that key — accepted for this copy-only migration pattern.
migrateNonceRow ::
  ( PGConstraints r,
    Member TinyLog r,
    Member Resource r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  (UserId, Text, Nonce) ->
  Maybe Int32 ->
  Sem r ()
migrateNonceRow migOpts migCounter migDuration (uid, key, nonce) mttl
  | maybe True (<= 0) mttl =
      debug $
        Log.msg (Log.val "skipping expired nonce") . Log.field "key" (Text.unpack key)
  | otherwise = do
      -- The guard above guarantees a positive TTL.
      let ttl = NonceTtlSecs . fromIntegral $ fromMaybe 1 mttl
      outcomeRef <- liftIO $ IORef.newIORef @Text "error"
      bracket
        (liftIO getCurrentTime)
        (observeDuration migDuration outcomeRef)
        ( const $ do
            timeoutResult <-
              Conc.timeout (migOpts.timeout <$ handleTimeout) migOpts.timeout $
                Postgres.insertNonce ttl uid key nonce
            case timeoutResult of
              Left timedOutAfter -> do
                markOutcome outcomeRef "timeout"
                liftIO . UnliftIO.throwIO $ MigrationTimedOut key timedOutAfter
              Right () -> do
                markOutcome outcomeRef "success"
                liftIO $ Prometheus.incCounter migCounter
        )
  where
    handleTimeout =
      err $
        Log.msg (Log.val "nonce migration timed out")
          . Log.field "key" (Text.unpack key)
          . Log.field "timeout" (show migOpts.timeout)

    markOutcome ref outcome = liftIO $ IORef.writeIORef ref outcome

    observeDuration metric outcomeRef start = do
      outcome <- liftIO $ IORef.readIORef outcomeRef
      end <- liftIO getCurrentTime
      liftIO $ Prometheus.withLabel metric outcome (`Prometheus.observe` realToFrac (diffUTCTime end start))

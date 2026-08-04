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

module Wire.PasswordResetCodeStore.Migration
  ( migratePasswordResetLoop,
  )
where

import Cassandra
import Data.ByteString.Conversion
import Data.Conduit
import Data.Conduit.List qualified as C
import Data.Id (UserId)
import Data.IORef qualified as IORef
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
import Wire.API.User.Password
import Wire.Migration
import Wire.PasswordResetCodeStore
import Wire.PasswordResetCodeStore.Postgres qualified as Postgres
import Wire.Postgres
import Wire.Sem.Logger (mapLogger)
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)

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

migratePasswordResetLoop ::
  MigrationOptions ->
  ClientState ->
  Hasql.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  IO ()
migratePasswordResetLoop migOpts cassClient pgPool logger migCounter migFinished migFailed migDuration =
  migrationLoop
    logger
    "password reset"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "password reset")
    (migrateAllPasswordReset migOpts migCounter migDuration)

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

migrateAllPasswordReset ::
  ( Member (Input Hasql.Pool) r,
    Member (Input ClientState) r,
    Member (Embed IO) r,
    Member TinyLog r,
    Member (State Int) r,
    Member Resource r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  ConduitM () Void (Sem r) ()
migrateAllPasswordReset migOpts migCounter migDuration = do
  lift $ info $ Log.msg (Log.val "migrateAllPasswordReset")
  withCount (paginateSem selectAllPasswordReset (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| C.mapM_ (traverse_ (\row@(key, _, _, _, _, _) -> handleErrors (toByteString' key) (migratePasswordResetRow migOpts migCounter migDuration row)))

migratePasswordResetRow ::
  ( PGConstraints r,
    Member TinyLog r,
    Member Resource r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  (PasswordResetKey, PasswordResetCode, UserId, Maybe Int32, Maybe UTCTime, Int32) ->
  Sem r ()
migratePasswordResetRow migOpts migCounter migDuration (key, code, uid, mRetries, mTimeout, ttl) =
  when (ttl > 0) $ case (mRetries, mTimeout) of
    (Just retries, Just codeTimeout) -> migrateRow retries codeTimeout
    -- retries/timeout are always present on rows written by CodeInsert (Identity), so a
    -- null here means a malformed/legacy row: skip it (logged) rather than block the
    -- migration. Its reset code is not backfilled, so the affected user re-initiates the
    -- reset after cutover.
    _ ->
      warn $
        Log.msg (Log.val "password reset row with null retries/timeout, skipping")
          . Log.field "key" (show key)
  where
    migrateRow retries codeTimeout = do
      let prqd =
            PRQueryData
              { prqdCode = code,
                prqdUser = uid,
                prqdRetries = Identity retries,
                prqdTimeout = Identity codeTimeout
              }
          keyText = T.pack (show key)
      outcomeRef <- liftIO $ IORef.newIORef @Text "error"
      bracket
        (liftIO getCurrentTime)
        (observeDuration migDuration outcomeRef)
        ( const $ do
            timeoutResult <-
              Conc.timeout (migOpts.timeout <$ handleTimeout) migOpts.timeout $
                Postgres.interpretPasswordResetCodeStoreToPostgres $
                  codeInsert key prqd ttl
            case timeoutResult of
              Left timedOutAfter -> do
                markOutcome outcomeRef "timeout"
                liftIO . UnliftIO.throwIO $ MigrationTimedOut keyText timedOutAfter
              Right () -> do
                markOutcome outcomeRef "success"
                liftIO $ Prometheus.incCounter migCounter
        )
    handleTimeout =
      err $
        Log.msg (Log.val "password reset code migration timed out")
          . Log.field "key" (show key)
          . Log.field "timeout" (show migOpts.timeout)
    markOutcome ref outcome = liftIO $ IORef.writeIORef ref outcome
    observeDuration metric outcomeRef start = do
      outcome <- liftIO $ IORef.readIORef outcomeRef
      end <- liftIO getCurrentTime
      liftIO $ Prometheus.withLabel metric outcome (`Prometheus.observe` realToFrac (diffUTCTime end start))

selectAllPasswordReset ::
  PrepQuery
    R
    ()
    (PasswordResetKey, PasswordResetCode, UserId, Maybe Int32, Maybe UTCTime, Int32)
selectAllPasswordReset =
  "SELECT key, code, user, retries, timeout, ttl(code) FROM password_reset"

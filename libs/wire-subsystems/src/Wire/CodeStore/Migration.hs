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

module Wire.CodeStore.Migration (migrateCodesLoop) where

import Cassandra hiding (Value)
import Data.ByteString.Conversion
import Data.Code (Key, Value)
import Data.Conduit
import Data.Conduit.List qualified as C
import Data.IORef qualified as IORef
import Data.Id (ConvId)
import Data.Misc (HttpsUrl)
import Data.Text qualified as T
import Data.Time
import Hasql.Pool qualified as Hasql
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
import Wire.API.Password
import Wire.CodeStore
import Wire.CodeStore.Cassandra.Queries qualified as Cql
import Wire.CodeStore.Code
import Wire.CodeStore.Postgres qualified as Postgres
import Wire.Migration
import Wire.Postgres
import Wire.Sem.Logger (mapLogger)
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)

type EffectStack =
  [ State Int,
    Input ClientState,
    Input Hasql.Pool,
    Input (Either HttpsUrl (Map Text HttpsUrl)),
    Resource,
    Async,
    Race,
    TinyLog,
    Embed IO,
    Final IO
  ]

migrateCodesLoop ::
  MigrationOptions ->
  ClientState ->
  Hasql.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  IO ()
migrateCodesLoop migOpts cassClient pgPool logger migCounter migFinished migFailed migDuration =
  migrationLoop
    logger
    "conversation codes"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "conversation codes")
    (migrateAllCodes migOpts migCounter migDuration)

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
    . runInputConst (Right mempty)
    . runInputConst pgPool
    . runInputConst cassClient
    . runState 0

migrateAllCodes ::
  ( Member (Input Hasql.Pool) r,
    Member (Input (Either HttpsUrl (Map Text HttpsUrl))) r,
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
migrateAllCodes migOpts migCounter migDuration = do
  lift $ info $ Log.msg (Log.val "migrateAllCodes")
  withCount (paginateSem Cql.selectAllCodes (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| C.mapM_ (traverse_ (\row@(key, _, _, _, _) -> handleErrors (toByteString' key) (migrateCodeRow migOpts migCounter migDuration row)))

migrateCodeRow ::
  ( Member (Input (Either HttpsUrl (Map Text HttpsUrl))) r,
    PGConstraints r,
    Member TinyLog r,
    Member Resource r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  (Key, Value, Int32, ConvId, Maybe Password) ->
  Sem r ()
migrateCodeRow migOpts migCounter migDuration (k, v, ttl, cnv, mPw) =
  when (ttl > 0) $ do
    let (code, _) = toCode k (v, ttl, cnv, mPw)
        keyText = T.pack (show k)
    outcomeRef <- liftIO $ IORef.newIORef @Text "error"
    bracket
      (liftIO getCurrentTime)
      (observeDuration migDuration outcomeRef)
      ( const $ do
          timeoutResult <- Conc.timeout (migOpts.timeout <$ handleTimeout) migOpts.timeout $ Postgres.interpretCodeStoreToPostgres $ createCode code mPw
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
        Log.msg (Log.val "conversation code migration timed out")
          . Log.field "key" (show k)
          . Log.field "timeout" (show migOpts.timeout)

    markOutcome ref outcome = liftIO $ IORef.writeIORef ref outcome

    observeDuration metric outcomeRef start = do
      outcome <- liftIO $ IORef.readIORef outcomeRef
      end <- liftIO getCurrentTime
      liftIO $ Prometheus.withLabel metric outcome (`Prometheus.observe` realToFrac (diffUTCTime end start))

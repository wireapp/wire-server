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

module Wire.ActivationCodeStore.Migration (migrateActivationKeysLoop) where

import Cassandra hiding (Value)
import Data.ByteString.Conversion
import Data.Conduit
import Data.Conduit.List qualified as C
import Data.Id (UserId)
import Data.Time
import Hasql.Pool.Extended qualified as Hasql
import Imports
import Polysemy
import Polysemy.Async
import Polysemy.Conc (interpretRace)
import Polysemy.Conc.Effect.Race hiding (Timeout)
import Polysemy.Input
import Polysemy.Resource (Resource, resourceToIOFinal)
import Polysemy.State
import Polysemy.TinyLog
import Prometheus qualified
import System.Logger qualified as Log
import Wire.API.User.Activation
import Wire.ActivationCodeStore.Postgres qualified as Postgres
import Wire.Migration
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

migrateActivationKeysLoop ::
  MigrationOptions ->
  ClientState ->
  Hasql.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  IO ()
migrateActivationKeysLoop migOpts cassClient pgPool logger migCounter migFinished migFailed migDuration =
  migrationLoop
    logger
    "activation keys"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "activation keys")
    (migrateAllActivationKeys migOpts migCounter migDuration)

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

migrateAllActivationKeys ::
  ( Member (Input Hasql.Pool) r,
    Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r,
    Member (State Int) r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  ConduitM () Void (Sem r) ()
migrateAllActivationKeys migOpts migCounter migDuration = do
  lift $ info $ Log.msg (Log.val "migrateAllActivationKeys")
  withCount (paginateSem selectAllActivationKeys (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| C.mapM_ (traverse_ (\row@(key, _, _, _, _, _) -> handleErrors (toByteString' key) (migrateActivationKeyRow migCounter migDuration row)))

migrateActivationKeyRow ::
  (PGConstraints r) =>
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  (ActivationKey, Text, ActivationCode, Maybe UserId, Int32, Int32) ->
  Sem r ()
migrateActivationKeyRow migCounter migDuration (key, keyText, code, mUser, retries, ttl) =
  when (ttl > 0) $ do
    start <- liftIO getCurrentTime
    Postgres.interpretActivationCodeStoreToPostgres $
      Postgres.insertActivationKeyRow (key, "email", keyText, code, mUser, retries, ttl)
    end <- liftIO getCurrentTime
    liftIO $ Prometheus.withLabel migDuration "success" (`Prometheus.observe` realToFrac (diffUTCTime end start))
    liftIO $ Prometheus.incCounter migCounter

selectAllActivationKeys :: PrepQuery R () (ActivationKey, Text, ActivationCode, Maybe UserId, Int32, Int32)
selectAllActivationKeys =
  "SELECT key, key_text, code, user, retries, ttl(code) FROM activation_keys"

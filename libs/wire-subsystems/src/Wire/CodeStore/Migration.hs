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

module Wire.CodeStore.Migration
  ( MigrationOptions (..),
    migrateCodesLoop,
  )
where

import Cassandra hiding (Value)
import Data.Code (Key, Value)
import Data.Conduit
import Data.Conduit.List qualified as C
import Data.Id (ConvId)
import Data.Misc (HttpsUrl)
import Hasql.Pool qualified as Hasql
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.State
import Polysemy.TinyLog
import Prometheus qualified
import System.Logger qualified as Log
import UnliftIO.Exception qualified as UnliftIO
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
  IO ()
migrateCodesLoop migOpts cassClient pgPool logger migCounter migFinished migFailed =
  migrationLoop cassClient pgPool logger migFinished migFailed (migrateAllCodes migOpts migCounter)

migrationLoop ::
  ClientState ->
  Hasql.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  ConduitT () Void (Sem EffectStack) () ->
  IO ()
migrationLoop cassClient pgPool logger migFinished migFailed migration = do
  go `UnliftIO.catch` handleIOError
  where
    handleIOError :: SomeException -> IO ()
    handleIOError exc = do
      Prometheus.incCounter migFailed
      Log.err logger $
        Log.msg (Log.val "migration failed, it won't restart unless the background-worker is restarted.")
          . Log.field "migration" (Log.val "conversation-codes")
          . Log.field "error" (displayException exc)
      UnliftIO.throwIO exc

    go :: IO ()
    go =
      runMigration >>= \case
        0 -> do
          Prometheus.incCounter migFinished
          Log.info logger $
            Log.msg (Log.val "finished migration")
              . Log.field "migration" (Log.val "conversation-codes")
        failed -> do
          Prometheus.incCounter migFailed
          Log.warn logger $
            Log.msg (Log.val "finished migration with errors")
              . Log.field "migration" (Log.val "conversation-codes")
              . Log.field "failed" failed

    runMigration :: IO Int
    runMigration =
      fmap fst
        . interpreter cassClient pgPool logger
        $ runConduit migration

interpreter :: ClientState -> Hasql.Pool -> Log.Logger -> Sem EffectStack a -> IO (Int, a)
interpreter cassClient pgPool logger =
  runFinal
    . embedToFinal
    . loggerToTinyLog logger
    . mapLogger (Log.field "migration" (Log.val "conversation-codes") .)
    . raiseUnder
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
    Member (State Int) r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  ConduitM () Void (Sem r) ()
migrateAllCodes migOpts migCounter = do
  lift $ info $ Log.msg (Log.val "migrateAllCodes")
  withCount (paginateSem Cql.selectAllCodes (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| C.mapM_ (traverse_ (handleErrors (migrateCodeRow migCounter)))

handleErrors ::
  ( Member (State Int) r,
    Member TinyLog r
  ) =>
  ((Key, Scope, Value, Int32, ConvId, Maybe Password) -> Sem (Error Hasql.UsageError : r) ()) ->
  (Key, Scope, Value, Int32, ConvId, Maybe Password) ->
  Sem r ()
handleErrors action row@(k, s, _, _, _, _) = do
  eithErr <- runError (action row)
  case eithErr of
    Right _ -> pure ()
    Left e -> do
      warn $
        Log.msg (Log.val "error occurred during migration")
          . Log.field "key" (show k)
          . Log.field "scope" (show s)
          . Log.field "error" (show e)
      modify (+ 1)

migrateCodeRow ::
  ( Member (Input (Either HttpsUrl (Map Text HttpsUrl))) r,
    PGConstraints r
  ) =>
  Prometheus.Counter ->
  (Key, Scope, Value, Int32, ConvId, Maybe Password) ->
  Sem r ()
migrateCodeRow migCounter (k, s, v, ttl, cnv, mPw) =
  when (ttl > 0) $ do
    let (code, _) = toCode k s (v, ttl, cnv, mPw)
    Postgres.interpretCodeStoreToPostgres $ createCode code mPw
    liftIO $ Prometheus.incCounter migCounter

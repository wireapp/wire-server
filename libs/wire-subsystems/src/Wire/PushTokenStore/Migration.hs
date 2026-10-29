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

module Wire.PushTokenStore.Migration (migratePushTokensLoop) where

import Cassandra hiding (Value)
import Data.ByteString.Conversion (fromByteString)
import Data.Conduit
import Data.Conduit.List qualified as C
import Data.ByteString.Lazy qualified as LBS
import Data.Id
import Data.IORef qualified as IORef
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (diffUTCTime, getCurrentTime)
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
import Polysemy.Resource (Resource, bracket, resourceToIOFinal)
import Polysemy.TinyLog
import Prometheus qualified
import System.Logger qualified as Log
import UnliftIO qualified
import Wire.API.PostgresMarshall
import Wire.API.Push.V2 (AppName (..), Token (..))
import Wire.Migration
import Wire.Postgres
import Wire.PushTokenStore.Postgres qualified as Postgres
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

migratePushTokensLoop ::
  MigrationOptions ->
  ClientState ->
  Hasql.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  IO ()
migratePushTokensLoop migOpts cassClient pgPool logger migCounter migFinished migFailed migDuration =
  migrationLoop
    logger
    "push-tokens"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "push-tokens")
    (migrateAllPushTokens migOpts migCounter migDuration)

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

-- | Full scan of the Cassandra user_push table, defined module-locally (this
-- store has no Queries module). Push tokens have no TTL, so every row is
-- copied.
selectAllPushTokens :: PrepQuery R () (UUID, Int32, Text, Text, Maybe Text, Blob, Maybe Text)
selectAllPushTokens =
  "SELECT usr, transport, app, ptoken, arn, connection, client FROM user_push"

migrateAllPushTokens ::
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
migrateAllPushTokens migOpts migCounter migDuration = do
  lift $ info $ Log.msg (Log.val "migrateAllPushTokens: user_push")
  withCount (paginateSem selectAllPushTokens (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| C.mapM_
      ( traverse_ $ \row@(usr, trp, app, tok, _, _, _) ->
          handleErrors (TE.encodeUtf8 (rowKeyText (usr, trp, app, tok))) $
            migratePushTokenRow migOpts migCounter migDuration row
      )

-- | Log/display key for a row, shared by the error handler and the
-- timeout/error messages so they stay correlated. The push token itself is
-- a device credential and is never logged; (usr, transport, app) plus the
-- token's length identify the row for operators.
rowKeyText :: (UUID, Int32, Text, Text) -> Text
rowKeyText (usr, trp, app, tok) =
  T.pack (show (usr, trp, app)) <> " tokenlen=" <> T.pack (show (T.length tok))

-- | Copy one @user_push@ row. Push tokens have no TTL, so the row is copied
-- verbatim.
migratePushTokenRow ::
  ( Member TinyLog r,
    Member Resource r,
    Member Race r,
    PGConstraints r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  (UUID, Int32, Text, Text, Maybe Text, Blob, Maybe Text) ->
  Sem r ()
migratePushTokenRow migOpts migCounter migDuration (usr, trp, app, tok, arn, blob, client) =
  case postgresUnmarshall trp of
    Left terr ->
      err $
        Log.msg (Log.val "skipping push token with undecodable transport")
          . Log.field "row" (rowKeyText (usr, trp, app, tok))
          . Log.field "error" terr
    Right transport ->
      withTimeoutAndMetrics migOpts migCounter migDuration (rowKeyText (usr, trp, app, tok)) $
        Postgres.insertPushTokenRow
          (Id usr)
          transport
          (AppName app)
          (Token tok)
          arn
          (ConnId (LBS.toStrict (fromBlob blob)))
          clientId
  where
    clientId = client >>= fromByteString . TE.encodeUtf8

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
        Log.msg (Log.val "push token migration timed out")
          . Log.field "row" row
          . Log.field "timeout" (show migOpts.timeout)

    markOutcome ref outcome = liftIO $ IORef.writeIORef ref outcome

    observeDuration metric outcomeRef start = do
      outcome <- liftIO $ IORef.readIORef outcomeRef
      end <- liftIO getCurrentTime
      liftIO $ Prometheus.withLabel metric outcome (`Prometheus.observe` realToFrac (diffUTCTime end start))

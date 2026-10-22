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

-- | Copy-only migration of the Cassandra @team@ table (team metadata) to
-- Postgres.  There is no delete-after-copy: @team@ is a small row-per-team
-- table, and rows are only ever mutated (never garbage collected) — the
-- upsert keeps re-runs idempotent.
module Wire.TeamStore.Migration (migrateTeamLoop) where

import Cassandra hiding (Value)
import Cassandra.Util (Writetime, writetimeToUTC)
import Data.Conduit
import Data.Conduit.List qualified as C
import Data.IORef qualified as IORef
import Data.Id
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time
import Data.UUID (UUID)
import Hasql.Pool.Extended qualified as Hasql
import Hasql.Statement qualified as Hasql
import Hasql.TH
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
import Wire.API.Routes.Internal.Galley.TeamsIntra
import Wire.API.Team
import Wire.API.Team.SearchVisibility
import Wire.ConversationStore.Cassandra.Instances ()
import Wire.Migration
import Wire.Postgres
import Wire.Sem.Logger (mapLogger)
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)
import Wire.TeamStore.Cassandra.Queries qualified as Cql

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

migrateTeamLoop ::
  MigrationOptions ->
  ClientState ->
  Hasql.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  IO ()
migrateTeamLoop migOpts cassClient pgPool logger migCounter migFinished migFailed migDuration =
  migrationLoop
    logger
    "teams"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "teams")
    (migrateAllTeams migOpts migCounter migDuration)

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

migrateAllTeams ::
  ( Member (Input Hasql.Pool) r,
    Member (Input ClientState) r,
    Member (Embed IO) r,
    Member TinyLog r,
    Member (AtomicState Int) r,
    Member Resource r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  ConduitM () Void (Sem r) ()
migrateAllTeams migOpts migCounter migDuration = do
  lift $ info $ Log.msg (Log.val "migrateAllTeams")
  withCount (paginateSem Cql.selectAllTeams (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| C.mapM_ (traverse_ (\row@(tid, _, _, _, _, _, _, _, _, _, _) -> handleErrors (T.encodeUtf8 (T.pack (show tid))) (migrateTeamRow migOpts migCounter migDuration row)))

migrateTeamRow ::
  ( PGConstraints r,
    Member TinyLog r,
    Member Resource r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  ( UUID,
    UserId,
    Text,
    Icon,
    Maybe Text,
    Bool,
    Maybe TeamStatus,
    Maybe TeamBinding,
    Maybe Icon,
    Maybe TeamSearchVisibility,
    Maybe (Writetime TeamBinding)
  ) ->
  Sem r ()
migrateTeamRow migOpts migCounter migDuration row@(tid, _, _, _, _, _, _, _, _, _, _) = do
  let keyText = T.pack (show tid)
  outcomeRef <- liftIO $ IORef.newIORef @Text "error"
  bracket
    (liftIO getCurrentTime)
    (observeDuration migDuration outcomeRef)
    ( const $ do
        timeoutResult <- Conc.timeout (migOpts.timeout <$ handleTimeout) migOpts.timeout $ upsertTeamRow row
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
        Log.msg (Log.val "team migration timed out")
          . Log.field "team" (show tid)
          . Log.field "timeout" (show migOpts.timeout)

    markOutcome ref outcome = liftIO $ IORef.writeIORef ref outcome

    observeDuration metric outcomeRef start = do
      outcome <- liftIO $ IORef.readIORef outcomeRef
      end <- liftIO getCurrentTime
      liftIO $ Prometheus.withLabel metric outcome (`Prometheus.observe` realToFrac (diffUTCTime end start))

-- | Upserts one copied team row.  Missing Cassandra columns fall back to the
-- same defaults the read paths use (status @active@, binding @nonBinding@),
-- and @created_at@ is carried over from @writetime(binding)@ so that
-- 'GetTeamCreationTime' survives the migration.  On conflict the Cassandra
-- row (source of truth) wins, except for a @created_at@ already present from
-- an earlier copy.
upsertTeamRow ::
  (PGConstraints r) =>
  ( UUID,
    UserId,
    Text,
    Icon,
    Maybe Text,
    Bool,
    Maybe TeamStatus,
    Maybe TeamBinding,
    Maybe Icon,
    Maybe TeamSearchVisibility,
    Maybe (Writetime TeamBinding)
  ) ->
  Sem r ()
upsertTeamRow (tid, creator, name, icon, iconKey, deleted, status, binding, splash, searchVis, mwBinding) =
  runStatement
    ( tid,
      Just (toUUID creator),
      name,
      postgresMarshall icon,
      iconKey,
      deleted,
      postgresMarshall (fromMaybe Active status),
      postgresMarshall (fromMaybe NonBinding binding),
      postgresMarshall <$> splash,
      postgresMarshall <$> searchVis,
      writetimeToUTC <$> mwBinding
    )
    upsertTeamStatement

upsertTeamStatement ::
  Hasql.Statement
    (UUID, Maybe UUID, Text, Text, Maybe Text, Bool, Int32, Bool, Maybe Text, Maybe Int32, Maybe UTCTime)
    ()
upsertTeamStatement =
  [resultlessStatement|
    insert into team (team, creator, name, icon, icon_key, deleted, status, binding, splash_screen, search_visibility, created_at)
    values ($1 :: uuid, $2 :: uuid?, $3 :: text, $4 :: text, $5 :: text?, $6 :: bool, $7 :: int, $8 :: bool, $9 :: text?, $10 :: int?, $11 :: timestamptz?)
    on conflict (team) do update set
      creator = excluded.creator,
      name = excluded.name,
      icon = excluded.icon,
      icon_key = excluded.icon_key,
      deleted = excluded.deleted,
      status = excluded.status,
      binding = excluded.binding,
      splash_screen = excluded.splash_screen,
      search_visibility = excluded.search_visibility,
      created_at = coalesce(excluded.created_at, team.created_at)
  |]

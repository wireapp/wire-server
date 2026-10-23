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

-- | Copy-only migration of team invitations and pending SCIM users from
-- Cassandra to Postgres.  The denormalized @team_invitation_info@ and
-- @team_invitation_email@ Cassandra tables are not copied: the Postgres
-- @team_invitation@ table covers them with plain indexes on @code@ and
-- @email@.
module Wire.InvitationStore.Migration
  ( migrateInvitationsLoop,
  )
where

import Cassandra hiding (Value)
import Data.Conduit
import Data.Conduit.List qualified as C
import Data.IORef qualified as IORef
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Data.UUID (UUID)
import Data.UUID qualified
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
import Wire.API.Team.Role (Role)
import Wire.API.User (EmailAddress)
import Wire.InvitationStore.Postgres qualified as Postgres
import Wire.Migration
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

migrateInvitationsLoop ::
  MigrationOptions ->
  ClientState ->
  Hasql.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  IO ()
migrateInvitationsLoop migOpts cassClient pgPool logger migCounter migFinished migFailed migDuration =
  migrationLoop
    logger
    "invitations"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "invitations")
    ( migrateAllInvitations migOpts migCounter migDuration
        >> migrateAllPendingScimUsers migOpts migCounter migDuration
    )

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

migrateAllInvitations ::
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
migrateAllInvitations migOpts migCounter migDuration = do
  lift $ info $ Log.msg (Log.val "migrateAllInvitations")
  withCount (paginateSem selectAllInvitations (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| C.mapM_ (traverse_ (\row@(team, _, _, _, _, _, _, _, _) -> handleErrors (encodeUtf8 (Data.UUID.toText team)) (migrateInvitationRow migOpts migCounter migDuration row)))

migrateInvitationRow ::
  ( PGConstraints r,
    Member TinyLog r,
    Member Resource r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  (UUID, Maybe Role, UUID, UTCTime, Maybe UUID, EmailAddress, Maybe Text, Text, Int32) ->
  Sem r ()
migrateInvitationRow migOpts migCounter migDuration row@(team, _, invId, _, _, _, _, _, ttl) =
  -- Rows with an expired or missing TTL have already vanished or will vanish
  -- from Cassandra on their own; skip them.
  when (ttl > 0) $
    migrateRowWithTimeout
      migOpts
      migCounter
      migDuration
      "invitation"
      (rowKey (team, invId))
      (Postgres.insertInvitationQ (rowToInvitationRow row))
  where
    rowKey (t, i) = encodeUtf8 (Data.UUID.toText t <> "/" <> Data.UUID.toText i)
    rowToInvitationRow (t, role, i, createdAt, createdBy, email, name, code, ttlSecs) =
      ( t,
        i,
        postgresMarshall <$> role,
        createdAt,
        createdBy,
        postgresMarshall email,
        name,
        code,
        ttlSecs
      )

migrateAllPendingScimUsers ::
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
migrateAllPendingScimUsers migOpts migCounter migDuration = do
  lift $ info $ Log.msg (Log.val "migrateAllPendingScimUsers")
  withCount (paginateSem selectAllPendingScimUsers (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| C.mapM_ (traverse_ (\row@(team, _, _) -> handleErrors (encodeUtf8 (Data.UUID.toText team)) (migratePendingScimUserRow migOpts migCounter migDuration row)))

migratePendingScimUserRow ::
  ( PGConstraints r,
    Member TinyLog r,
    Member Resource r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  (UUID, Text, UUID) ->
  Sem r ()
migratePendingScimUserRow migOpts migCounter migDuration row@(team, _, _) =
  migrateRowWithTimeout
    migOpts
    migCounter
    migDuration
    "pending scim user"
    ((encodeUtf8 (Data.UUID.toText team)))
    (Postgres.insertPendingScimUserQ row)

-- | Copy one row to Postgres under the per-row migration timeout, recording
-- the outcome in the duration histogram and incrementing the migrated counter
-- on success.
migrateRowWithTimeout ::
  ( PGConstraints r,
    Member TinyLog r,
    Member Resource r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  -- | Human-readable row name for the timeout log line.
  Text ->
  -- | Key identifying the row, for logs and timeout errors.
  ByteString ->
  -- | The Postgres insert to perform.
  Sem r () ->
  Sem r ()
migrateRowWithTimeout migOpts migCounter migDuration what keyTextKey insert = do
  let keyText = T.pack (show keyTextKey)
  outcomeRef <- liftIO $ IORef.newIORef @Text "error"
  bracket
    (liftIO getCurrentTime)
    (observeDuration migDuration outcomeRef)
    ( const $ do
        timeoutResult <-
          Conc.timeout (migOpts.timeout <$ handleTimeout) migOpts.timeout insert
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
        Log.msg (Log.val (encodeUtf8 (what <> " migration timed out")))
          . Log.field "key" (show keyTextKey)
          . Log.field "timeout" (show migOpts.timeout)
    markOutcome ref outcome = liftIO $ IORef.writeIORef ref outcome
    observeDuration metric outcomeRef start = do
      outcome <- liftIO $ IORef.readIORef outcomeRef
      end <- liftIO getCurrentTime
      liftIO $ Prometheus.withLabel metric outcome (`Prometheus.observe` realToFrac (diffUTCTime end start))

selectAllInvitations ::
  PrepQuery
    R
    ()
    (UUID, Maybe Role, UUID, UTCTime, Maybe UUID, EmailAddress, Maybe Text, Text, Int32)
selectAllInvitations =
  "SELECT team, role, id, created_at, created_by, email, name, code, ttl(created_at) FROM team_invitation"

selectAllPendingScimUsers ::
  PrepQuery
    R
    ()
    (UUID, Text, UUID)
selectAllPendingScimUsers =
  "SELECT team, email, user FROM team_scim_pending_user_email"

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
import Data.Id (Id (..), TeamId)
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
import Polysemy.Conc.Effect.Race hiding (Timeout)
import Polysemy.Error (Error)
import Polysemy.Input
import Polysemy.Resource (Resource, resourceToIOFinal)
import Polysemy.TinyLog
import Prometheus qualified
import System.Logger qualified as Log
import Wire.API.PostgresMarshall
import Wire.API.Team.Role (Role)
import Wire.API.User (EmailAddress)
import Wire.InvitationStore.Postgres qualified as Postgres
import Wire.Migration
import Wire.MigrationLock
import Wire.Postgres
import Wire.Sem.Logger (mapLogger)
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)
import Wire.Util (embedClient)

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
    Member Async r,
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
    .| C.mapM_ (traverse_ (\row@(team, _, invId, _, _, _, _, _, _) -> handleLockAndDBErrors (encodeUtf8 (Data.UUID.toText team <> "/" <> Data.UUID.toText invId)) (migrateInvitationRow migOpts migCounter migDuration row)))

migrateInvitationRow ::
  ( PGConstraints r,
    Member (Input ClientState) r,
    Member TinyLog r,
    Member Async r,
    Member (Error MigrationLockError) r,
    Member Race r,
    Member Resource r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  (UUID, Maybe Role, UUID, UTCTime, Maybe UUID, EmailAddress, Maybe Text, Text, Int32) ->
  Sem r ()
migrateInvitationRow migOpts migCounter migDuration (team, _, invId, _, _, _, _, _, _) =
  -- The payload is re-read from Cassandra inside the exclusive lock: the
  -- snapshot row this function was called with may have been deleted
  -- concurrently, and copying it would resurrect it in Postgres.
  void . withExclusiveMigrationLockAndTimeout migOpts.timeout migDuration [Id team :: TeamId] $ do
    client <- input @ClientState
    mRow <-
      embedClient client $
        retry x1 (query1 selectInvitationForMigration (params LocalQuorum (team, invId)))
    for_ mRow $ \(teamId, role, rowId, createdAt, createdBy, email, name, code, ttlSecs) ->
      -- Expired rows have already vanished from Cassandra; skip them.
      when (ttlSecs > 0) $ do
        Postgres.insertInvitationImpl (teamId, rowId, postgresMarshall <$> role, createdAt, createdBy, postgresMarshall email, name, code, ttlSecs)
        liftIO $ Prometheus.incCounter migCounter

migrateAllPendingScimUsers ::
  ( Member (Input Hasql.Pool) r,
    Member (Input ClientState) r,
    Member (Embed IO) r,
    Member TinyLog r,
    Member (AtomicState Int) r,
    Member Async r,
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
    .| C.mapM_ (traverse_ (\row@(team, _, _) -> handleLockAndDBErrors (encodeUtf8 (Data.UUID.toText team)) (migratePendingScimUserRow migOpts migCounter migDuration row)))

migratePendingScimUserRow ::
  ( PGConstraints r,
    Member (Input ClientState) r,
    Member TinyLog r,
    Member Async r,
    Member (Error MigrationLockError) r,
    Member Race r,
    Member Resource r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  (UUID, Text, UUID) ->
  Sem r ()
migratePendingScimUserRow migOpts migCounter migDuration (team, email, _) =
  -- Re-read inside the lock so a pending user deleted mid-migration is not
  -- copied to Postgres.
  void . withExclusiveMigrationLockAndTimeout migOpts.timeout migDuration [(Id team :: TeamId, postgresMarshall email)] $ do
    client <- input @ClientState
    rows <-
      embedClient client $
        retry x1 (query selectPendingScimUserForMigration (params LocalQuorum (team, email)))
    for_ rows $ \(_, email', user) -> do
      Postgres.insertPendingScimUserImpl (team, email', user)
      liftIO $ Prometheus.incCounter migCounter

selectAllInvitations ::
  PrepQuery
    R
    ()
    (UUID, Maybe Role, UUID, UTCTime, Maybe UUID, EmailAddress, Maybe Text, Text, Int32)
selectAllInvitations =
  "SELECT team, role, id, created_at, created_by, email, name, code, ttl(created_at) FROM team_invitation"

selectInvitationForMigration ::
  PrepQuery
    R
    (UUID, UUID)
    (UUID, Maybe Role, UUID, UTCTime, Maybe UUID, EmailAddress, Maybe Text, Text, Int32)
selectInvitationForMigration =
  "SELECT team, role, id, created_at, created_by, email, name, code, ttl(created_at) FROM team_invitation WHERE team = ? AND id = ?"

selectPendingScimUserForMigration ::
  PrepQuery
    R
    (UUID, Text)
    (UUID, Text, UUID)
selectPendingScimUserForMigration =
  "SELECT team, email, user FROM team_scim_pending_user_email WHERE team = ? AND email = ?"

selectAllPendingScimUsers ::
  PrepQuery
    R
    ()
    (UUID, Text, UUID)
selectAllPendingScimUsers =
  "SELECT team, email, user FROM team_scim_pending_user_email"

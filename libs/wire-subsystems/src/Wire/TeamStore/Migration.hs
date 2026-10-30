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
--
-- 'migrateTeamMembersLoop' additionally migrates the member tables
-- (@team_member@, @user_team@, @billing_team_member@, @team_admin@), also
-- copy-only: the listing effects that read those tables in Cassandra
-- ('TeamMemberStore', the team listings) are not migrated yet, so deleting
-- the Cassandra rows would empty them before their readers move to
-- Postgres.  The copy runs under per-(team, user) exclusive locks; the
-- dual-write interpreter takes the matching shared locks on member writes.
module Wire.TeamStore.Migration
  ( migrateTeamLoop,
    migrateTeamMembersLoop,
  )
where

import Cassandra hiding (Value)
import Cassandra.Util (Writetime, writetimeToUTC)
import Data.Conduit
import Data.Conduit.List qualified as C
import Data.IORef qualified as IORef
import Data.Id
import Data.Json.Util (UTCTimeMillis (..), fromUTCTimeMillis)
import Data.LegalHold (UserLegalHoldStatus)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time
import Data.UUID (UUID)
import Hasql.Pool qualified as HasqlPool
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
import Polysemy.Error (Error, runError)
import Polysemy.Input
import Polysemy.Resource (Resource, bracket, resourceToIOFinal)
import Polysemy.TinyLog
import Prometheus qualified
import System.Logger qualified as Log
import UnliftIO qualified
import Wire.API.PostgresMarshall
import Wire.API.Routes.Internal.Galley.TeamsIntra
import Wire.API.Team
import Wire.API.Team.Permission (Permissions)
import Wire.API.Team.SearchVisibility
import Wire.ConversationStore.Cassandra.Instances ()
import Wire.Migration
import Wire.MigrationLock
import Wire.Postgres
import Wire.Sem.Logger (mapLogger)
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)
import Wire.TeamStore.Cassandra.Queries qualified as Cql
import Wire.TeamStore.Postgres qualified as Postgres
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
    .| C.mapM_ (traverse_ (\row@(tid, _, _, _, _, _, _, _, _, _, _, _) -> handleErrors (T.encodeUtf8 (T.pack (show tid))) (migrateTeamRow migOpts migCounter migDuration row)))

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
    Maybe (Writetime TeamStatus),
    Maybe TeamBinding,
    Maybe (Writetime TeamBinding),
    Maybe Icon,
    Maybe TeamSearchVisibility
  ) ->
  Sem r ()
migrateTeamRow migOpts migCounter migDuration row@(tid, _, _, _, _, _, _, _, _, _, _, _) = do
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

currentTimeStamp :: UTCTime
currentTimeStamp = UTCTime (ModifiedJulianDay 0) 0

-- | Upserts one copied team row.  Missing Cassandra columns fall back to the
-- same defaults the read paths use (status @active@, binding @nonBinding@),
-- and Cassandra's writetimes are carried over into the maintained
-- @status_time@/@binding_time@ columns so that @tdStatusTime@ and
-- 'GetTeamCreationTime' survive the migration.  On conflict the Cassandra
-- row (source of truth) wins.
upsertTeamRow ::
  (PGConstraints r) =>
  ( UUID,
    UserId,
    Text,
    Icon,
    Maybe Text,
    Bool,
    Maybe TeamStatus,
    Maybe (Writetime TeamStatus),
    Maybe TeamBinding,
    Maybe (Writetime TeamBinding),
    Maybe Icon,
    Maybe TeamSearchVisibility
  ) ->
  Sem r ()
upsertTeamRow (tid, creator, name, icon, iconKey, deleted, status, mwStatus, binding, mwBinding, splash, searchVis) =
  runStatement
    ( tid,
      Just (toUUID creator),
      name,
      postgresMarshall icon,
      iconKey,
      deleted,
      postgresMarshall (fromMaybe Active status),
      maybe currentTimeStamp writetimeToUTC mwStatus,
      postgresMarshall (fromMaybe NonBinding binding),
      maybe currentTimeStamp writetimeToUTC mwBinding,
      postgresMarshall <$> splash,
      postgresMarshall <$> searchVis
    )
    upsertTeamStatement

upsertTeamStatement ::
  Hasql.Statement
    (UUID, Maybe UUID, Text, Text, Maybe Text, Bool, Text, UTCTime, Text, UTCTime, Maybe Text, Maybe Text)
    ()
upsertTeamStatement =
  [resultlessStatement|
    insert into team (team, creator, name, icon, icon_key, deleted, status, status_time, binding, binding_time, splash_screen, search_visibility)
    values ($1 :: uuid, $2 :: uuid?, $3 :: text, $4 :: text, $5 :: text?, $6 :: bool, $7 :: text, $8 :: timestamptz, $9 :: text, $10 :: timestamptz, $11 :: text?, $12 :: text?)
    on conflict (team) do update set
      creator = excluded.creator,
      name = excluded.name,
      icon = excluded.icon,
      icon_key = excluded.icon_key,
      deleted = excluded.deleted,
      status = excluded.status,
      status_time = excluded.status_time,
      binding = excluded.binding,
      binding_time = excluded.binding_time,
      splash_screen = excluded.splash_screen,
      search_visibility = excluded.search_visibility
  |]

--------------------------------------------------------------------------------
-- Team members (WPB-22957): copy + delete-after-copy

-- | Copies the member tables (Cassandra -> Postgres) and deletes the
-- Cassandra rows after each member (plus its auxiliary rows) has been
-- written to Postgres, under a per-(team, user) advisory lock so concurrent
-- writes and the copy do not race.  Rows mutate (permissions, legalhold
-- status) and disappear (members leave, team deletion sweeps members), so
-- delete-after-copy is what eventually empties Cassandra.
migrateTeamMembersLoop ::
  MigrationOptions ->
  ClientState ->
  Hasql.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  IO ()
migrateTeamMembersLoop migOpts cassClient pgPool logger migCounter migFinished migFailed migDuration =
  migrationLoop
    logger
    "team members"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "team members")
    (migrateAllTeamMembers migOpts migCounter migDuration)

migrateAllTeamMembers ::
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  ConduitM () Void (Sem EffectStack) ()
migrateAllTeamMembers migOpts migCounter migDuration = do
  lift $ info $ Log.msg (Log.val "migrateAllTeamMembers")
  withCount (paginateSem selectAllTeamMembers (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| C.mapM_
      ( traverse_ $ \(tid, uid, perms, mwPerms, minvu, minvt, mlh) -> do
          eith <-
            runError @HasqlPool.UsageError
              ( runError @MigrationLockError
                  ( migrateTeamMemberRow
                      migOpts
                      migCounter
                      migDuration
                      ( toUUID tid,
                        toUUID uid,
                        postgresMarshall perms,
                        writetimeToUTC mwPerms,
                        toUUID <$> minvu,
                        fromUTCTimeMillis <$> minvt,
                        Postgres.userLegalHoldStatusToText <$> mlh
                      )
                  )
              )
          case eith of
            Right (Right ()) -> pure ()
            errs -> do
              warn $
                Log.msg (Log.val "error occurred during team member migration")
                  . Log.field "key" (idToText tid <> ":" <> idToText uid)
                  . Log.field "error" (show errs)
              atomicModify (+ 1)
      )

-- | Copies one member row and its auxiliary rows to Postgres, under the
-- per-(team, user) lock and the configured per-row timeout.  The Cassandra
-- rows are kept (copy-only): the listing effects that read them
-- (TeamMemberStore, the team listings) are not migrated yet, so deleting
-- would empty them before their readers move to Postgres.  The exclusive
-- lock serializes the copy against the shared locks the dual-write
-- interpreter takes on member writes, so a concurrent permission change can
-- not be overwritten by a stale copy.
migrateTeamMemberRow ::
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
  ( UUID,
    UUID,
    (Int64, Int64),
    UTCTime,
    Maybe UUID,
    Maybe UTCTime,
    Maybe Text
  ) ->
  Sem r ()
migrateTeamMemberRow migOpts migCounter migDuration row@(tid, uid, _, _, _, _, _) =
  withExclusiveMigrationLockAndTimeout
    migOpts.timeout
    migDuration
    [(Id tid :: TeamId, Id uid :: UserId)]
    $ do
      upsertTeamMemberRow row
      copyAuxiliaryTeamMemberRows (Id tid) (Id uid)
      liftIO $ Prometheus.incCounter migCounter

-- | Reads the auxiliary rows of one member from Cassandra and copies the
-- ones that exist to Postgres.
copyAuxiliaryTeamMemberRows ::
  ( PGConstraints r,
    Member (Input ClientState) r
  ) =>
  TeamId ->
  UserId ->
  Sem r ()
copyAuxiliaryTeamMemberRows tid uid = do
  mUserTeam <-
    embedClientInput $
      retry x1 (query1 selectUserTeamExact (params LocalQuorum (uid, tid)))
  for_ mUserTeam $ \_ -> runStatement (toUUID uid, toUUID tid) upsertUserTeamStatement
  mBilling <-
    embedClientInput $
      retry x1 (query1 selectBillingTeamMemberExact (params LocalQuorum (tid, uid)))
  for_ mBilling $ \_ -> runStatement (toUUID tid, toUUID uid) upsertBillingTeamMemberStatement
  mAdmin <-
    embedClientInput $
      retry x1 (query1 selectTeamAdminExact (params LocalQuorum (tid, uid)))
  for_ mAdmin $ \_ -> runStatement (toUUID tid, toUUID uid) upsertTeamAdminStatement

-- | Upserts exactly what Cassandra had for this member row; the Cassandra
-- @writetime(perms)@ is carried over into the maintained @perms_write_time@
-- column so 'teamMemberInfosPg' keeps serving the original permissions write
-- time after the copy.
upsertTeamMemberRow ::
  (PGConstraints r) =>
  (UUID, UUID, (Int64, Int64), UTCTime, Maybe UUID, Maybe UTCTime, Maybe Text) ->
  Sem r ()
upsertTeamMemberRow (tid, uid, (ps, pc), mwPerms, minvu, minvt, mlh) =
  runStatement
    ( tid,
      uid,
      ps,
      pc,
      mwPerms,
      minvu,
      minvt,
      mlh
    )
    upsertTeamMemberStatement

upsertTeamMemberStatement ::
  Hasql.Statement
    (UUID, UUID, Int64, Int64, UTCTime, Maybe UUID, Maybe UTCTime, Maybe Text)
    ()
upsertTeamMemberStatement =
  [resultlessStatement|
    insert into team_member (team, user_id, perms_self, perms_copy, perms_write_time, invited_by, invited_at, legalhold_status)
    values ($1 :: uuid, $2 :: uuid, $3 :: int8, $4 :: int8, $5 :: timestamptz, $6 :: uuid?, $7 :: timestamptz?, $8 :: text?)
    on conflict (team, user_id) do update set
      perms_self = excluded.perms_self,
      perms_copy = excluded.perms_copy,
      perms_write_time = excluded.perms_write_time,
      invited_by = excluded.invited_by,
      invited_at = excluded.invited_at,
      legalhold_status = excluded.legalhold_status
  |]

upsertUserTeamStatement :: Hasql.Statement (UUID, UUID) ()
upsertUserTeamStatement =
  [resultlessStatement|
    insert into user_team (user_id, team) values ($1 :: uuid, $2 :: uuid)
    on conflict (user_id, team) do nothing
  |]

upsertBillingTeamMemberStatement :: Hasql.Statement (UUID, UUID) ()
upsertBillingTeamMemberStatement =
  [resultlessStatement|
    insert into billing_team_member (team, user_id) values ($1 :: uuid, $2 :: uuid)
    on conflict (team, user_id) do nothing
  |]

upsertTeamAdminStatement :: Hasql.Statement (UUID, UUID) ()
upsertTeamAdminStatement =
  [resultlessStatement|
    insert into team_admin (team, user_id) values ($1 :: uuid, $2 :: uuid)
    on conflict (team, user_id) do nothing
  |]

--------------------------------------------------------------------------------
-- local scan queries (bound at use site so the Cql/Tuple instances resolve,
-- mirroring Wire.ConversationStore.Migration)

selectAllTeamMembers :: PrepQuery R () (TeamId, UserId, Permissions, Writetime Permissions, Maybe UserId, Maybe UTCTimeMillis, Maybe UserLegalHoldStatus)
selectAllTeamMembers = "select team, user, perms, writetime(perms), invited_by, invited_at, legalhold_status from team_member"

selectUserTeamExact :: PrepQuery R (UserId, TeamId) (Identity TeamId)
selectUserTeamExact = "select team from user_team where user = ? and team = ?"

selectBillingTeamMemberExact :: PrepQuery R (TeamId, UserId) (Identity UserId)
selectBillingTeamMemberExact = "select user from billing_team_member where team = ? and user = ?"

selectTeamAdminExact :: PrepQuery R (TeamId, UserId) (Identity UserId)
selectTeamAdminExact = "select user from team_admin where team = ? and user = ?"

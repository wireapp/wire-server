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
-- insert-if-absent plus writetime-guarded reconciliation keeps re-runs
-- idempotent.
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
import Data.Vector qualified as V
import Hasql.Pool qualified as HasqlPool
import Hasql.Pool.Extended qualified as Hasql
import Imports
import Polysemy
import Polysemy.Async
import Polysemy.AtomicState
import Polysemy.Conc (interpretRace)
import Polysemy.Conc qualified as Conc
import Polysemy.Conc.Effect.Race hiding (Timeout)
import Polysemy.Error (runError)
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
    .| C.mapM_ (traverse_ (\row@(tid, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> handleErrors (T.encodeUtf8 (T.pack (show tid))) (migrateTeamRow migOpts migCounter migDuration row)))

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
    Maybe (Writetime Text),
    Icon,
    Maybe (Writetime Icon),
    Maybe Text,
    Maybe (Writetime Text),
    Bool,
    Maybe TeamStatus,
    Maybe (Writetime TeamStatus),
    Maybe TeamBinding,
    Maybe (Writetime TeamBinding),
    Maybe Icon,
    Maybe (Writetime Icon),
    Maybe TeamSearchVisibility,
    Maybe (Writetime TeamSearchVisibility)
  ) ->
  Sem r ()
migrateTeamRow migOpts migCounter migDuration row@(tid, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = do
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

-- | Copies one team row into Postgres with version-guarded reconciliation:
-- the snapshot is inserted only if the row is absent, and each column whose
-- Cassandra writetime is present is then reconciled with the same
-- last-write-wins guard the dual write uses
-- (@*col*_time is null or *col*_time <= writetime@).  A stale snapshot page
-- can therefore never overwrite a newer dual-written value.  Columns whose
-- Cassandra column has no writetime are only set by the insert-if-absent
-- (first copy of the row).
upsertTeamRow ::
  (PGConstraints r) =>
  ( UUID,
    UserId,
    Text,
    Maybe (Writetime Text),
    Icon,
    Maybe (Writetime Icon),
    Maybe Text,
    Maybe (Writetime Text),
    Bool,
    Maybe TeamStatus,
    Maybe (Writetime TeamStatus),
    Maybe TeamBinding,
    Maybe (Writetime TeamBinding),
    Maybe Icon,
    Maybe (Writetime Icon),
    Maybe TeamSearchVisibility,
    Maybe (Writetime TeamSearchVisibility)
  ) ->
  Sem r ()
upsertTeamRow (tid, creator, name, mwName, icon, mwIcon, iconKey, mwIconKey, _deleted, status, mwStatus, binding, mwBinding, splash, mwSplash, searchVis, mwSearchVis) = do
  Postgres.insertTeamRowIfAbsentPg
    (Id tid)
    creator
    name
    (writetimeToUTC <$> mwName)
    (postgresMarshall icon)
    (writetimeToUTC <$> mwIcon)
    iconKey
    (writetimeToUTC <$> mwIconKey)
    (postgresMarshall <$> splash)
    (writetimeToUTC <$> mwSplash)
    (fromMaybe Active status)
    (writetimeToUTC <$> mwStatus)
    (fromMaybe NonBinding binding)
    (writetimeToUTC <$> mwBinding)
    searchVis
    (writetimeToUTC <$> mwSearchVis)
  for_ mwName $ \w -> Postgres.updateTeamNamePg (Id tid) name (writetimeToUTC w)
  for_ mwIcon $ \w -> Postgres.updateTeamIconPg (Id tid) (postgresMarshall icon) (writetimeToUTC w)
  for_ mwIconKey $ \w -> Postgres.updateTeamIconKeyPg (Id tid) iconKey (writetimeToUTC w)
  for_ mwSplash $ \w -> for_ splash $ \ss -> Postgres.updateTeamSplashScreenPg (Id tid) (postgresMarshall ss) (writetimeToUTC w)
  for_ mwStatus $ \w -> Postgres.updateTeamStatusPg (Id tid) (fromMaybe Active status) (writetimeToUTC w)
  for_ mwSearchVis $ \w -> Postgres.updateTeamSearchVisibilityPg (Id tid) v (writetimeToUTC w)
  where
    v = fromMaybe SearchVisibilityStandard searchVis

--------------------------------------------------------------------------------
-- Team members (WPB-22957): copy-only

-- | Copies the member tables (Cassandra -> Postgres).  Cassandra rows are
-- NOT deleted: like the other stores, deletion is deferred to the final
-- cutover, and the copy loop is idempotent (Cassandra is the source of
-- truth while migrating).
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
    .| C.mapM_ (migrateTeamMemberPage migOpts migCounter migDuration)

-- | Copies one page of member rows and their auxiliary rows to Postgres
-- under a single batched exclusive lock (@withMigrationLocks@ sorts the
-- keys, so concurrent pages can not deadlock).  All Cassandra reads happen
-- inside the locked section: the scan page is only used as the key set, and
-- every member row is re-read point-wise inside the lock so a stale page
-- can never overwrite a newer dual write, and rows deleted from Cassandra
-- before the lock is taken are skipped instead of resurrected.
migrateTeamMemberPage ::
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  [(TeamId, UserId, Permissions, Writetime Permissions, Maybe UserId, Maybe UTCTimeMillis, Maybe UserLegalHoldStatus)] ->
  Sem EffectStack ()
migrateTeamMemberPage migOpts migCounter migDuration page = do
  eith <-
    runError @HasqlPool.UsageError . runError @MigrationLockError $
      withExclusiveMigrationLockAndTimeout
        migOpts.timeout
        migDuration
        [(tid, uid) | (tid, uid, _, _, _, _, _) <- page]
        $ do
          -- Re-read the member rows inside the locked section (5685 pattern).
          for_ page $ \(tid, uid, _, _, _, _, _) -> do
            mrow <-
              embedClientInput $
                retry x1 (query1 selectTeamMemberExact (params LocalQuorum (tid, uid)))
            for_ mrow $ \(perms, mwPerms, minvu, minvt, mlh) -> do
              let (permsSelf, permsCopy) = postgresMarshall perms
              runStatement
                ( toUUID tid,
                  toUUID uid,
                  permsSelf,
                  permsCopy,
                  toUUID <$> minvu,
                  fromUTCTimeMillis <$> minvt,
                  Postgres.userLegalHoldStatusToText <$> mlh,
                  writetimeToUTC mwPerms
                )
                Postgres.upsertTeamMemberRowStatement
            liftIO $ Prometheus.incCounter migCounter
          -- Auxiliary rows: batched IN reads inside the same page lock, then
          -- bulk UNNEST upserts (one round trip per table instead of per row).
          let uids = V.fromList (toUUID . (\(_, u, _, _, _, _, _) -> u) <$> page)
              tids = V.fromList (toUUID . (\(t, _, _, _, _, _, _) -> t) <$> page)
          (mUserTeams, mBilling, mAdmins) <-
            embedClientInput $
              (,,)
                <$> retry x1 (query selectUserTeamsForUsers (params LocalQuorum (Identity (V.toList uids))))
                <*> retry x1 (query selectBillingTeamMembersForTeams (params LocalQuorum (Identity (V.toList tids))))
                <*> retry x1 (query selectTeamAdminsForTeams (params LocalQuorum (Identity (V.toList tids))))
          runStatement (V.fromList (fst <$> mUserTeams), V.fromList (snd <$> mUserTeams)) Postgres.upsertUserTeamsPgStatement
          runStatement (V.fromList (fst <$> mBilling), V.fromList (snd <$> mBilling)) Postgres.upsertBillingTeamMembersPgStatement
          runStatement (V.fromList (fst <$> mAdmins), V.fromList (snd <$> mAdmins)) Postgres.upsertTeamAdminsPgStatement
  case eith of
    Right (Right ()) -> pure ()
    errs -> do
      warn (Log.msg (Log.val "error occurred during team member migration") . Log.field "error" (show errs))
      atomicModify (+ 1)


--------------------------------------------------------------------------------
-- local scan queries (bound at use site so the Cql/Tuple instances resolve,
-- mirroring Wire.ConversationStore.Migration)

selectAllTeamMembers :: PrepQuery R () (TeamId, UserId, Permissions, Writetime Permissions, Maybe UserId, Maybe UTCTimeMillis, Maybe UserLegalHoldStatus)
selectAllTeamMembers = "select team, user, perms, writetime(perms), invited_by, invited_at, legalhold_status from team_member"

selectTeamMemberExact :: PrepQuery R (TeamId, UserId) (Permissions, Writetime Permissions, Maybe UserId, Maybe UTCTimeMillis, Maybe UserLegalHoldStatus)
selectTeamMemberExact = "select perms, writetime(perms), invited_by, invited_at, legalhold_status from team_member where team = ? and user = ?"

selectUserTeamsForUsers :: PrepQuery R (Identity [UUID]) (UUID, UUID)
selectUserTeamsForUsers = "select user, team from user_team where user in ?"

selectBillingTeamMembersForTeams :: PrepQuery R (Identity [UUID]) (UUID, UUID)
selectBillingTeamMembersForTeams = "select team, user from billing_team_member where team in ?"

selectTeamAdminsForTeams :: PrepQuery R (Identity [UUID]) (UUID, UUID)
selectTeamAdminsForTeams = "select team, user from team_admin where team in ?"

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
-- FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program.  If not, see <https://www.gnu.org/licenses/>.

-- | Copy-only Cassandra -> Postgres migration for the TeamStore tables
-- (@team@, @team_member@, @user_team@, @billing_team_member@, @team_admin@).
module Wire.TeamStore.Migration (migrateTeamStoreLoop) where

import Cassandra hiding (Value)
import Cassandra.Util (Writetime (..), writetimeToUTC)
import Data.Conduit
import Data.Conduit.List qualified as C
import Data.IORef qualified as IORef
import Data.Id
import Data.Json.Util (UTCTimeMillis (..), fromUTCTimeMillis)
import Data.LegalHold
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time
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
import Wire.API.PostgresMarshall (PostgresMarshall (..))
import Wire.API.Routes.Internal.Galley.TeamsIntra
import Wire.API.Team
import Wire.API.Team.Permission (Permissions)
import Wire.API.Team.SearchVisibility
import Wire.ConversationStore.Cassandra.Instances ()
import Wire.Migration
import Wire.Postgres (PGConstraints)
import Wire.Sem.Logger (mapLogger)
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)
import Wire.TeamStore.Postgres qualified as Postgres

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

migrateTeamStoreLoop ::
  MigrationOptions ->
  ClientState ->
  Hasql.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  IO ()
migrateTeamStoreLoop migOpts cassClient pgPool logger migCounter migFinished migFailed migDuration =
  migrationLoop
    logger
    "team store"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "team store")
    (migrateAllTeamStore migOpts migCounter migDuration)

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

type WithMigration r =
  ( Member (Input Hasql.Pool) r,
    Member (Input ClientState) r,
    Member (Embed IO) r,
    Member TinyLog r,
    Member (AtomicState Int) r,
    Member Resource r,
    Member Race r
  )

migrateAllTeamStore ::
  (WithMigration r) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  ConduitM () Void (Sem r) ()
migrateAllTeamStore migOpts migCounter migDuration = do
  lift $ info $ Log.msg (Log.val "migrateAllTeamStore")
  -- teams and members get per-row duration metrics (the bulk of the data);
  -- the three auxiliary tables are tiny.
  migrateTeams migOpts migCounter migDuration
  migrateTeamMembers migOpts migCounter migDuration
  migrateUserTeams migOpts
  migrateBillingTeamMembers migOpts
  migrateTeamAdmins migOpts

migrateTeams ::
  (WithMigration r) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  ConduitM () Void (Sem r) ()
migrateTeams migOpts migCounter migDuration =
  withCount (paginateSem selectTeams (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| C.mapM_
      ( traverse_ $ \(tid, creator, name, icon, iconKey, deleted, status, statusWT, binding, bindingWT, splashScreen, searchVisibility) ->
          handleErrors (encodeUtf8 (idToText tid)) $
            migrateRowWithMetrics
              migOpts
              migCounter
              migDuration
              (encodeUtf8 (idToText tid))
              ( Postgres.migrateTeamRow
                  Postgres.TeamRow
                    { Postgres.trCreator = toUUID creator,
                      Postgres.trName = name,
                      Postgres.trIcon = postgresMarshall icon,
                      Postgres.trIconKey = iconKey,
                      Postgres.trDeleted = deleted,
                      Postgres.trStatus = postgresMarshall <$> status,
                      -- carry the Cassandra writetimes over so the TeamData
                      -- status time and TeamCreationTime survive the copy
                      Postgres.trStatusTime = maybe currentTimeStamp writetimeToUTC statusWT,
                      Postgres.trBinding = postgresMarshall <$> binding,
                      Postgres.trBindingTime = maybe currentTimeStamp writetimeToUTC bindingWT,
                      Postgres.trSplashScreen = postgresMarshall <$> splashScreen,
                      Postgres.trSearchVisibility = postgresMarshall <$> searchVisibility
                    }
                  (toUUID tid)
              )
      )

migrateTeamMembers ::
  (WithMigration r) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  ConduitM () Void (Sem r) ()
migrateTeamMembers migOpts migCounter migDuration =
  withCount (paginateSem selectTeamMembers (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| C.mapM_
      ( traverse_ $ \(tid, uid, perms, minvu, minvt, mlh) ->
          handleErrors (encodeUtf8 (idToText tid)) $
            migrateRowWithMetrics
              migOpts
              migCounter
              migDuration
              (encodeUtf8 (idToText tid))
              ( let (permsSelf, permsCopy) = postgresMarshall perms :: (Int64, Int64)
                 in Postgres.migrateTeamMemberRow
                      (toUUID tid)
                      ( toUUID uid,
                        permsSelf,
                        permsCopy,
                        toUUID <$> minvu,
                        fromUTCTimeMillis <$> minvt,
                        Postgres.userLegalHoldStatusToText <$> mlh
                      )
              )
      )

migrateUserTeams :: (WithMigration r) => MigrationOptions -> ConduitM () Void (Sem r) ()
migrateUserTeams migOpts =
  withCount (paginateSem selectUserTeams (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| C.mapM_
      ( traverse_ $ \(u, t) ->
          handleErrors (encodeUtf8 (idToText u)) $
            Postgres.migrateUserTeamRow (toUUID u, toUUID t)
      )

migrateBillingTeamMembers :: (WithMigration r) => MigrationOptions -> ConduitM () Void (Sem r) ()
migrateBillingTeamMembers migOpts =
  withCount (paginateSem selectBillingTeamMembers (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| C.mapM_
      ( traverse_ $ \(t, u) ->
          handleErrors (encodeUtf8 (idToText t)) $
            Postgres.migrateBillingTeamMemberRow (toUUID t, toUUID u)
      )

migrateTeamAdmins :: (WithMigration r) => MigrationOptions -> ConduitM () Void (Sem r) ()
migrateTeamAdmins migOpts =
  withCount (paginateSem selectTeamAdmins (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| C.mapM_
      ( traverse_ $ \(t, u) ->
          handleErrors (encodeUtf8 (idToText t)) $
            Postgres.migrateTeamAdminRow (toUUID t, toUUID u)
      )

-- | Runs one row migration under the configured per-row timeout, recording the
-- duration/outcome in the metric and counting the row on success. Mirrors the
-- canonical Wire.CodeStore.Migration.migrateCodeRow.
migrateRowWithMetrics ::
  ( PGConstraints r,
    Member TinyLog r,
    Member Resource r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  ByteString ->
  Sem r () ->
  Sem r ()
migrateRowWithMetrics migOpts migCounter migDuration keyText action = do
  outcomeRef <- liftIO $ IORef.newIORef @Text "error"
  bracket
    (liftIO getCurrentTime)
    (observeDuration migDuration outcomeRef)
    ( const $ do
        timeoutResult <- Conc.timeout (migOpts.timeout <$ handleTimeout keyText) migOpts.timeout action
        case timeoutResult of
          Left timedOutAfter -> do
            liftIO $ IORef.writeIORef outcomeRef "timeout"
            liftIO . UnliftIO.throwIO $ MigrationTimedOut (decodeUtf8 keyText) timedOutAfter
          Right () -> do
            liftIO $ IORef.writeIORef outcomeRef "success"
            liftIO $ Prometheus.incCounter migCounter
    )
  where
    handleTimeout k =
      err $
        Log.msg (Log.val "team store migration timed out")
          . Log.field "key" (show k)
          . Log.field "timeout" (show migOpts.timeout)

observeDuration ::
  (Member (Embed IO) r) =>
  Prometheus.Vector Text Prometheus.Histogram ->
  IORef.IORef Text ->
  UTCTime ->
  Sem r ()
observeDuration metric outcomeRef start = do
  outcome <- liftIO $ IORef.readIORef outcomeRef
  end <- liftIO getCurrentTime
  liftIO $ Prometheus.withLabel metric outcome (`Prometheus.observe` realToFrac (diffUTCTime end start))

currentTimeStamp :: UTCTime
currentTimeStamp = UTCTime (ModifiedJulianDay 0) 0

--------------------------------------------------------------------------------
-- local scan queries (bound at use site so the Cql/Tuple instances resolve,
-- mirroring Wire.ConversationStore.Migration)

selectTeams :: PrepQuery R () (TeamId, UserId, Text, Icon, Maybe Text, Bool, Maybe TeamStatus, Maybe (Writetime TeamStatus), Maybe TeamBinding, Maybe (Writetime TeamBinding), Maybe Icon, Maybe TeamSearchVisibility)
selectTeams = "select team, creator, name, icon, icon_key, deleted, status, writetime(status), binding, writetime(binding), splash_screen, search_visibility from team"

selectTeamMembers :: PrepQuery R () (TeamId, UserId, Permissions, Maybe UserId, Maybe UTCTimeMillis, Maybe UserLegalHoldStatus)
selectTeamMembers = "select team, user, perms, invited_by, invited_at, legalhold_status from team_member"

selectUserTeams :: PrepQuery R () (UserId, TeamId)
selectUserTeams = "select user, team from user_team"

selectBillingTeamMembers :: PrepQuery R () (TeamId, UserId)
selectBillingTeamMembers = "select team, user from billing_team_member"

selectTeamAdmins :: PrepQuery R () (TeamId, UserId)
selectTeamAdmins = "select team, user from team_admin"

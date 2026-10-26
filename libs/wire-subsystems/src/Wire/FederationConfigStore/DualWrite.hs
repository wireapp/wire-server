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

module Wire.FederationConfigStore.DualWrite
  ( interpretFederationConfigStoreToCassandraAndPostgres,
  )
where

import Cassandra (ClientState)
import Data.Domain (Domain)
import Hasql.Pool qualified as Hasql
import Hasql.Pool.Extended qualified as HasqlPoolExt
import Imports
import Polysemy
import Polysemy.Async (Async, asyncToIOFinal)
import Polysemy.Conc (Race, interpretRace)
import Polysemy.Error (Error, runError)
import Polysemy.Input
import Polysemy.Resource (Resource, resourceToIOFinal)
import Polysemy.Time.Data.TimeUnit (MilliSeconds (MilliSeconds))
import System.Logger qualified as Log
import UnliftIO (throwIO)
import UnliftIO.Exception (throwString)
import Wire.API.Routes.FederationDomainConfig (FederationDomainConfig (..))
import Wire.FederationConfigStore
import Wire.FederationConfigStore qualified as FederationConfigStore
import Wire.FederationConfigStore.Cassandra qualified as Cassandra
import Wire.FederationConfigStore.Postgres qualified as Postgres
import Wire.MigrationLock
import Wire.Postgres
import Wire.Sem.Logger (Logger, mapLogger)
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)

-- | Cassandra is the source of truth during migration; writes are mirrored to
-- Postgres.  Mirrored writes acquire the shared migration lock for the domain,
-- pairing with the copy loop's exclusive-locked row copy in
-- Wire.FederationConfigStore.Migration.  The lock is acquired inside a
-- self-contained local effect run (like the migration loop's interpreter), so
-- this interpreter stays first-order and demands nothing beyond 'PGConstraints'
-- from the wiring stack.  A lock-acquisition timeout is surfaced as a
-- 'MigrationLockError' exception (a 500 for the client): the Cassandra write
-- has already been acknowledged, so a mirror must never be silently skipped.
interpretFederationConfigStoreToCassandraAndPostgres ::
  (PGConstraints r) =>
  ClientState ->
  Log.Logger ->
  InterpreterFor FederationConfigStore r
interpretFederationConfigStoreToCassandraAndPostgres cs logger = interpret $ \case
  GetFederationConfig d ->
    Cassandra.interpretFederationDomainConfig cs $ FederationConfigStore.getFederationConfig d
  GetFederationConfigs ->
    Cassandra.interpretFederationDomainConfig cs $ FederationConfigStore.getFederationConfigs
  AddFederationConfig cnf@FederationDomainConfig {domain} -> do
    res <- Cassandra.interpretFederationDomainConfig cs $ FederationConfigStore.addFederationConfig cnf
    -- Mirror only rows Cassandra accepted (e.g. not cap-rejected), so PG never
    -- holds rows the source of truth refused.
    case res of
      AddFederationRemoteSuccess ->
        mirrorShared logger domain (void (FederationConfigStore.addFederationConfig cnf))
      _ -> pure ()
    pure res
  UpdateFederationConfig cnf@FederationDomainConfig {domain} -> do
    res <- Cassandra.interpretFederationDomainConfig cs $ FederationConfigStore.updateFederationConfig cnf
    mirrorShared logger domain (void (FederationConfigStore.updateFederationConfig cnf))
    pure res
  AddFederationRemoteTeam d t -> do
    -- Cassandra's store always succeeds here (rejections live in the
    -- subsystem), so no result-guard is needed, unlike AddFederationConfig.
    res <- Cassandra.interpretFederationDomainConfig cs $ FederationConfigStore.addFederationRemoteTeam d t
    mirrorShared logger d (void (FederationConfigStore.addFederationRemoteTeam d t))
    pure res
  RemoveFederationRemoteTeam d t -> do
    Cassandra.interpretFederationDomainConfig cs $ FederationConfigStore.removeFederationRemoteTeam d t
    mirrorShared logger d (void (FederationConfigStore.removeFederationRemoteTeam d t))
  GetFederationRemoteTeams d ->
    Cassandra.interpretFederationDomainConfig cs $ FederationConfigStore.getFederationRemoteTeams d

-- | Run the Postgres mirror action under the shared migration lock for the
-- domain.  The action runs in a fully interpreted local stack, mirroring how
-- Wire.FederationConfigStore.Migration runs its copy row.
type MirrorRow =
  '[ Input HasqlPoolExt.Pool,
     Error Hasql.UsageError,
     Error MigrationLockError,
     Resource,
     Async,
     Race,
     Logger (Log.Msg -> Log.Msg),
     Logger (Log.Msg -> Log.Msg),
     Embed IO,
     Final IO
   ]

mirrorShared ::
  (PGConstraints r) =>
  Log.Logger ->
  Domain ->
  (forall x. Sem (FederationConfigStore ': x) ()) ->
  Sem r ()
mirrorShared logger d act = do
  pool <- input @HasqlPoolExt.Pool
  eith <-
    embed
      $ runFinal
        . embedToFinal
        . loggerToTinyLog logger
        . mapLogger id
        . interpretRace
        . asyncToIOFinal
        . resourceToIOFinal
        . runError @MigrationLockError
        . runError @Hasql.UsageError
        . runInputConst pool
      $ withMigrationLocks LockShared (MilliSeconds 500) [d] (action)
  case eith of
    Right (Right ()) -> pure ()
    Right (Left usageErr) -> throwString ("federation config dual-write mirror failed: " <> show usageErr)
    Left lockErr -> throwIO lockErr
  where
    action :: Sem MirrorRow ()
    action = Postgres.interpretFederationConfigStoreToPostgres act

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

module Wire.MlsKeyPackageStore.Migrating (interpretMlsKeyPackageStoreToCassandraAndPostgres) where

import Cassandra (ClientState)
import Data.Id (UserId)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Imports
import Polysemy
import Polysemy.Async
import Polysemy.Conc.Effect.Race (Race)
import Polysemy.Error (Error)
import Polysemy.Input (runInputConst)
import Polysemy.Resource (Resource)
import Polysemy.Time
import Polysemy.TinyLog
import Wire.MigrationLock
import Wire.MlsKeyPackageStore
import Wire.MlsKeyPackageStore.Cassandra
import Wire.MlsKeyPackageStore.Migration (lookupUserClients, migrateUserKeyPackages)
import Wire.MlsKeyPackageStore.Postgres
import Wire.Postgres

-- | Migration-mode interpreter: routes every effect for a user to either the
-- Postgres or the Cassandra interpreter based on the per-user
-- @mls_key_packages_migrated_users@ marker (same strategy as
-- 'Wire.UserStore.Cassandra.runAppropriateInterpreter').
--
-- For an unmarked user, @route@ performs an on-demand migration under the
-- user's exclusive migration lock: copy the user's Cassandra partitions into
-- Postgres, mark the user, then serve the request from Postgres. This closes
-- the straggler window: without it, an unmarked user's first key package
-- uploaded after the background worker's enumeration passed them would never
-- be copied and would be unreachable after cutover. A lock acquisition
-- timeout raises 'MigrationLockError' (surfaced as HTTP 500; the client
-- retries). There are no cross-store writes: an effect runs against exactly
-- one backend, so claimed (one-time-use) key packages can never resurface.
--
-- Note that this makes migration mode opt-in to migration activity even when
-- the background worker is disabled: being in @migration-to-postgresql@ mode
-- means every access migrates the user it touches.
--
-- 'LookupKeyPackagesBulk' stays marker-only: bulk reads of unmarked users
-- still hit Cassandra, which holds their data until their own routed effect
-- migrates them.
interpretMlsKeyPackageStoreToCassandraAndPostgres ::
  ( PGConstraints r,
    Member Async r,
    Member TinyLog r,
    Member (Error MigrationLockError) r,
    Member Resource r,
    Member Race r
  ) =>
  ClientState ->
  InterpreterFor MlsKeyPackageStore r
interpretMlsKeyPackageStoreToCassandraAndPostgres cas = interpret $ \case
  InsertKeyPackages u c kps ->
    route u $ insertKeyPackages u c kps
  LookupKeyPackages u c s ->
    route u $ lookupKeyPackages u c s
  LookupKeyPackagesBulk requests -> do
    let uids = Set.toList (Set.fromList [u | (u, _, _) <- requests])
    withMigrationLocks LockShared (Seconds 2) uids $ do
      pgUsers <-
        Set.fromList . catMaybes
          <$> traverse
            ( \u ->
                isUserKeyPackagesMigrated u >>= \case
                  True -> pure (Just u)
                  False -> pure Nothing
            )
            uids
      let (pgReqs, cassReqs) =
            foldr
              (\req@(u, _, _) (pg, cass) -> if Set.member u pgUsers then (req : pg, cass) else (pg, req : cass))
              ([], [])
              requests
      pgResult <- interpretMlsKeyPackageStoreToPostgres (lookupKeyPackagesBulk pgReqs)
      cassResult <- interpretMlsKeyPackageStoreToCassandra cas (lookupKeyPackagesBulk cassReqs)
      pure (Map.union pgResult cassResult)
  DeleteKeyPackages u c s rs ->
    route u $ deleteKeyPackages u c s rs
  DeleteAllKeyPackages u c ss ->
    route u $ deleteAllKeyPackages u c ss
  DeleteKeyPackage u c s r ->
    route u $ deleteKeyPackage u c s r
  where
    route :: forall r a. (PGConstraints r, Member Async r, Member TinyLog r, Member (Error MigrationLockError) r, Member Resource r, Member Race r) => UserId -> Sem (MlsKeyPackageStore ': r) a -> Sem r a
    route u action = do
      migrated <-
        withMigrationLocks LockShared (MilliSeconds 500) [u] $
          isUserKeyPackagesMigrated u
      unless migrated $ do
        -- On-demand migration: copy + mark under the exclusive lock. The
        -- shared lock is fully released before the exclusive acquisition (no
        -- lock upgrade). A copy failure before the mark leaves the user
        -- unmarked; the retried request re-runs the migration.
        clients <- runInputConst cas (lookupUserClients u)
        void $
          withMigrationLocks LockExclusive (Seconds 5) [u] . runInputConst cas $
            migrateUserKeyPackages u clients
      interpretMlsKeyPackageStoreToPostgres action

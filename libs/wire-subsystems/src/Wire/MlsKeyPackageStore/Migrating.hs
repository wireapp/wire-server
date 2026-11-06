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

import Data.Id (UserId)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Cassandra (ClientState)
import Imports
import Polysemy
import Polysemy.Async
import Polysemy.Conc.Effect.Race (Race)
import Polysemy.Error (Error)
import Polysemy.Resource (Resource)
import Polysemy.Time
import Polysemy.TinyLog
import Wire.MigrationLock
import Wire.MlsKeyPackageStore
import Wire.MlsKeyPackageStore.Cassandra
import Wire.MlsKeyPackageStore.Postgres
import Wire.Postgres

-- | Migration-mode interpreter: routes every effect for a user to either the
-- Postgres or the Cassandra interpreter based on the per-user
-- @mls_key_packages_migrated_users@ marker (same strategy as
-- 'Wire.UserStore.Cassandra.runAppropriateInterpreter'). A per-user shared
-- migration lock is held while routing so that it cannot race with the
-- background worker's exclusive lock + copy + marker transaction.
--
-- There are no cross-store writes: a store effect runs against exactly one
-- backend, so claimed (one-time-use) key packages can never resurface.
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
    route u action =
      withMigrationLocks LockShared (MilliSeconds 500) [u] $
        isUserKeyPackagesMigrated u >>= \case
          True -> interpretMlsKeyPackageStoreToPostgres action
          False -> interpretMlsKeyPackageStoreToCassandra cas action

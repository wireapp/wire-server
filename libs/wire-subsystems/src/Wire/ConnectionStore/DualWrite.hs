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

module Wire.ConnectionStore.DualWrite
  ( interpretConnectionStoreToCassandraAndPostgres,
  )
where

import Cassandra qualified as Cas
import Data.Id (Id (..), UserId)
import Data.Qualified (tUnqualified)
import Data.Range (fromRange)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Polysemy.Async (Async)
import Polysemy.Conc (Race)
import Polysemy.Error (Error)
import Polysemy.Resource (Resource)
import Polysemy.Time.Data.TimeUnit (MilliSeconds (..), Seconds (..), TimeUnit)
import Polysemy.TinyLog (TinyLog)
import Wire.ConnectionStore
import Wire.ConnectionStore.Cassandra (connectionStoreToCassandra, deleteConnectionsWithContacts)
import Wire.ConnectionStore.Postgres qualified as Postgres
import Wire.MigrationLock
import Wire.Postgres (PGConstraints, runStatement)
import Wire.Sem.Paging.Cassandra (InternalPaging)

-- | During the connections migration, reads and deletes are routed per @left@
-- user under shared migration locks while writes are mirrored to both stores:
--
-- * Migration state: a @left@ user is migrated iff it has a row in
--   @connection_migrated_users@. Only the copier writes that marker, under the
--   exclusive per-user lock, after both partitions were copied cleanly and
--   before they are cleared from Cassandra. Row presence in @connection@ /
--   @connection_remote@ is NOT a migration signal: dual writes create
--   Postgres rows for users the copier has not reached yet.
-- * Reads: the dual interpreter takes the shared per-user migration lock,
--   looks up the markers of the involved @left@ users, and serves each
--   user's partition from Postgres or Cassandra accordingly. This serializes
--   reads against the background copier.
-- * Deletes: dual deletes take the shared lock for every touched @left@
--   partition. Without it, the copier's timestamp-guarded upsert could
--   re-insert a row that a concurrent dual delete had just removed from both
--   stores — a deleted connection reappearing is privacy-relevant.
-- * Inserts and status updates take the shared lock on @self@ (the
--   partition they write) and always write Postgres. They write Cassandra
--   only while @self@ is unmigrated: a plain Cassandra @UPDATE@ on a cleared
--   partition would recreate a partial row (no @conv@), and the copier must
--   never re-copy such leftovers (it only clears partitions of marked users).
--
-- Residual window (accepted, analogous to the pagination edge below): a
-- contact created between the contact-list enumeration and the locked delete
-- is unlocked while its reverse row is deleted; if the copier is copying that
-- contact at the same time, it can re-insert that one row into Postgres.
-- Fully closing this would need tombstones, which the migration does not
-- have.
--
-- Accepted edge case: a multi-request pagination that straddles the moment a
-- user becomes migrated may continue on the other store for its remaining
-- pages, so that one pagination can see truncated or duplicated rows
-- ('RemoteConnectedUsersPaginated''s continuation is store-bound the same
-- way). No token tagging or sticky routing is added for this. Related and
-- equally accepted: a user whose copy is incomplete (a failed row delays the
-- clear until the next copier pass) stays unmarked and keeps being served
-- from Cassandra until the retry completes.
interpretConnectionStoreToCassandraAndPostgres ::
  forall r.
  ( Member (Embed Cas.Client) r,
    PGConstraints r,
    Member TinyLog r,
    Member (Error MigrationLockError) r,
    Member Async r,
    Member Race r,
    Member Resource r
  ) =>
  InterpreterFor (ConnectionStore InternalPaging) r
interpretConnectionStoreToCassandraAndPostgres = interpret $ \case
  InsertConnection self target rel qcnv ->
    dualWrite
      (tUnqualified self)
      (Postgres.interpretConnectionStoreToPostgres $ insertConnection self target rel qcnv)
      (connectionStoreToCassandra $ insertConnection self target rel qcnv)
  UpdateConnectionStatus self target status ->
    dualWrite
      (tUnqualified self)
      (Postgres.interpretConnectionStoreToPostgres $ updateConnectionStatus self target status)
      (connectionStoreToCassandra $ updateConnectionStatus self target status)
  DeleteConnections u ->
    -- Reverse rows (@left = contact, right = u@) live in the contacts'
    -- partitions, so every touched partition must be locked. The contact list
    -- is read from BOTH stores under the shared [u] lock: until u's Cassandra
    -- partitions are cleared, Cassandra ⊇ Postgres (PG may hold a partial
    -- copy while a copier pass is still retrying u); once cleared, Cassandra
    -- is empty and only Postgres knows the contacts. The union is therefore
    -- complete in every state, and the shared [u] lock pins both stores'
    -- views while enumerating (the copier holds the exclusive lock while
    -- copying a partition, so views cannot change under us).
    --
    -- The inner lock set excludes u: u's shared lock is already held by the
    -- outer section, and requesting it again from the inner section's
    -- separate session would queue behind a waiting exclusive (copier)
    -- request that itself waits for the outer lock — a self-deadlock that
    -- only ends at the lock timeout.
    withMigrationLocks LockShared (MilliSeconds 500) [u] $ do
      pgContacts <- Postgres.interpretConnectionStoreToPostgres $ lookupContactListWithRelation u
      cassContacts <- connectionStoreToCassandra $ lookupContactListWithRelation u
      let contacts = filter (/= u) . nub $ map fst (pgContacts <> cassContacts)
      withMigrationLocks LockShared (MilliSeconds 500) contacts $ do
        -- One pass over the union deletes every reverse row from Cassandra
        -- (including those of contacts only Postgres still knows once u's
        -- partitions are cleared) and clears u's own partitions.
        embed @Cas.Client $ deleteConnectionsWithContacts u contacts
        Postgres.interpretConnectionStoreToPostgres $ deleteConnections u
  DeleteRemoteConnections u rs ->
    withMigrationLocks LockShared (MilliSeconds 500) (fromRange rs) $ do
      connectionStoreToCassandra $ deleteRemoteConnections u rs
      Postgres.interpretConnectionStoreToPostgres $ deleteRemoteConnections u rs
  LookupConnection self target ->
    routeSingleRead
      (tUnqualified self)
      (\_ -> Postgres.interpretConnectionStoreToPostgres $ lookupConnection self target)
      (\_ -> connectionStoreToCassandra $ lookupConnection self target)
  LookupRelationWithHistory self target ->
    routeSingleRead
      (tUnqualified self)
      (\_ -> Postgres.interpretConnectionStoreToPostgres $ lookupRelationWithHistory self target)
      (\_ -> connectionStoreToCassandra $ lookupRelationWithHistory self target)
  LookupLocalConnections lfrom start size ->
    routeSingleRead
      (tUnqualified lfrom)
      (\_ -> Postgres.interpretConnectionStoreToPostgres $ lookupLocalConnections lfrom start size)
      (\_ -> connectionStoreToCassandra $ lookupLocalConnections lfrom start size)
  LookupLocalConnectionsPage self ps size ->
    routeSingleRead
      (tUnqualified self)
      (\_ -> Postgres.interpretConnectionStoreToPostgres $ lookupLocalConnectionsPage self ps size)
      (\_ -> connectionStoreToCassandra $ lookupLocalConnectionsPage self ps size)
  LookupRemoteConnectionsPage self ps size ->
    routeSingleRead
      (tUnqualified self)
      (\_ -> Postgres.interpretConnectionStoreToPostgres $ lookupRemoteConnectionsPage self ps size)
      (\_ -> connectionStoreToCassandra $ lookupRemoteConnectionsPage self ps size)
  LookupConnectionStatus froms tos ->
    routeMultiRead
      froms
      (\migrated -> Postgres.interpretConnectionStoreToPostgres $ lookupConnectionStatus migrated tos)
      (\cassOnly -> connectionStoreToCassandra $ lookupConnectionStatus cassOnly tos)
  LookupConnectionStatus' froms ->
    routeMultiRead
      froms
      (\migrated -> Postgres.interpretConnectionStoreToPostgres $ lookupConnectionStatus' migrated)
      (\cassOnly -> connectionStoreToCassandra $ lookupConnectionStatus' cassOnly)
  LookupLocalConnectionStatuses froms tos ->
    routeMultiRead
      froms
      (\migrated -> Postgres.interpretConnectionStoreToPostgres $ lookupLocalConnectionStatuses migrated tos)
      (\cassOnly -> connectionStoreToCassandra $ lookupLocalConnectionStatuses cassOnly tos)
  LookupRemoteConnectionStatuses froms tos ->
    routeMultiRead
      froms
      (\migrated -> Postgres.interpretConnectionStoreToPostgres $ lookupRemoteConnectionStatuses migrated tos)
      (\cassOnly -> connectionStoreToCassandra $ lookupRemoteConnectionStatuses cassOnly tos)
  LookupAllStatuses lfroms ->
    routeMultiRead
      (tUnqualified lfroms)
      (\migrated -> Postgres.interpretConnectionStoreToPostgres $ lookupAllStatuses (migrated <$ lfroms))
      (\cassOnly -> connectionStoreToCassandra $ lookupAllStatuses (cassOnly <$ lfroms))
  RemoteConnectedUsersPaginated uid mps bounds ->
    routeSingleRead
      (tUnqualified uid)
      (\_ -> Postgres.interpretConnectionStoreToPostgres $ remoteConnectedUsersPaginated uid mps bounds)
      (\_ -> connectionStoreToCassandra $ remoteConnectedUsersPaginated uid mps bounds)
  LookupContactListWithRelation u ->
    routeSingleRead
      u
      (\_ -> Postgres.interpretConnectionStoreToPostgres $ lookupContactListWithRelation u)
      (\_ -> connectionStoreToCassandra $ lookupContactListWithRelation u)
  CountConnections u r ->
    routeSingleRead
      (tUnqualified u)
      (\_ -> Postgres.interpretConnectionStoreToPostgres $ countConnections u r)
      (\_ -> connectionStoreToCassandra $ countConnections u r)

-- | Mirror a write to the @left@ partition of @self@: under the shared lock,
-- always write Postgres, and write Cassandra only if @self@ is not migrated
-- yet. Returns the Cassandra result while unmigrated (unchanged behaviour),
-- the Postgres result afterwards.
dualWrite ::
  ( PGConstraints r,
    Member TinyLog r,
    Member (Error MigrationLockError) r,
    Member Async r,
    Member Race r,
    Member Resource r
  ) =>
  UserId ->
  Sem r a ->
  Sem r a ->
  Sem r a
dualWrite self pgAction cassAction =
  withMigrationLocks LockShared (MilliSeconds 500) [self] $ do
    migrated <- filterMigrated [self]
    if null migrated
      then do
        res <- cassAction
        _ <- pgAction
        pure res
      else pgAction

-- | Serve a read whose partitions are keyed by a single @left@ user: under
-- the shared lock, the whole read goes to Postgres if the user is migrated,
-- else to Cassandra.
routeSingleRead ::
  ( PGConstraints r,
    Member TinyLog r,
    Member (Error MigrationLockError) r,
    Member Async r,
    Member Race r,
    Member Resource r
  ) =>
  UserId ->
  ([UserId] -> Sem r a) ->
  ([UserId] -> Sem r a) ->
  Sem r a
routeSingleRead left pgAction cassAction =
  withMigrationLocks LockShared (MilliSeconds 500) [left] $ do
    migrated <- filterMigrated [left]
    if null migrated then cassAction [left] else pgAction [left]

-- | Serve a read whose partitions span several @left@ users: under the shared
-- lock, split the users into migrated and unmigrated subsets and run each
-- store on its subset. Guards both stores against empty subsets (Cassandra
-- errors on an empty @IN ?@).
routeMultiRead ::
  ( Monoid a,
    PGConstraints r,
    Member TinyLog r,
    Member (Error MigrationLockError) r,
    Member Async r,
    Member Race r,
    Member Resource r
  ) =>
  [UserId] ->
  ([UserId] -> Sem r a) ->
  ([UserId] -> Sem r a) ->
  Sem r a
routeMultiRead = routeRead (Seconds 2)

-- | Shared core of the read routing: acquire shared locks, ask Postgres which
-- of the @left@ users are migrated, and run each store on its subset.
routeRead ::
  forall a u r.
  ( Monoid a,
    TimeUnit u,
    PGConstraints r,
    Member TinyLog r,
    Member (Error MigrationLockError) r,
    Member Async r,
    Member Race r,
    Member Resource r
  ) =>
  u ->
  [UserId] ->
  ([UserId] -> Sem r a) ->
  ([UserId] -> Sem r a) ->
  Sem r a
routeRead maxWait leftUsers pgPart cassPart =
  withMigrationLocks LockShared maxWait leftUsers $ do
    migrated <- filterMigrated leftUsers
    let cassOnly = leftUsers \\ migrated
    pgRes <- if null migrated then pure mempty else pgPart migrated
    cassRes <- if null cassOnly then pure mempty else cassPart cassOnly
    pure (pgRes <> cassRes)

-- | Users from the given list that the copier has marked as migrated.
filterMigrated :: (PGConstraints r) => [UserId] -> Sem r [UserId]
filterMigrated leftUsers = do
  migrated <- runStatement (Vector.map toUUID (Vector.fromList leftUsers)) selectMigratedUsers
  pure (Id <$> Vector.toList migrated)

selectMigratedUsers :: Hasql.Statement (Vector UUID) (Vector UUID)
selectMigratedUsers =
  [vectorStatement|SELECT (user_id :: uuid) FROM connection_migrated_users WHERE user_id = ANY ($1 :: uuid[])|]

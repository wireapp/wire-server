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
import Wire.ConnectionStore.Cassandra (connectionStoreToCassandra, deleteIncomingConnections)
import Wire.ConnectionStore.Postgres qualified as Postgres
import Wire.MigrationLock
import Wire.Postgres (PGConstraints, runStatement)
import Wire.Sem.Paging.Cassandra (InternalPaging)

-- | During the connections migration, reads and deletes are routed per @left@
-- user under shared migration locks while writes are mirrored to both stores:
--
-- * Reads: the dual interpreter takes the shared per-user migration lock, asks
--   Postgres which of the involved @left@ users have already been migrated
--   (row presence in @connection@ / @connection_remote@), and serves each
--   user's partition from Postgres or Cassandra accordingly. This serializes
--   reads against the background copier, which copies a user's partitions and
--   deletes them from Cassandra under the exclusive lock.
-- * Deletes: dual deletes take the shared lock for every touched @left@
--   partition. Without it, the copier's timestamp-guarded upsert could
--   re-insert a row that a concurrent dual delete had just removed from both
--   stores — a deleted connection reappearing is privacy-relevant.
--   Inserts and status updates are lock-free: they cannot resurrect deleted
--   data because the copier's guarded upsert loses to newer dual-write
--   timestamps.
--
-- Residual window (accepted, analogous to the pagination edge below): a
-- contact created between the contact-list enumeration and the locked delete
-- is unlocked while its reverse row is deleted; the copier can re-insert that
-- one row into Postgres, and in the migrated case the Cassandra row becomes
-- an orphan until the contact's own migration re-copies it. Fully closing
-- this would need tombstones, which the migration does not have.
--
-- Accepted edge case: a multi-request pagination that straddles the moment a
-- user becomes migrated may continue on the other store for its remaining
-- pages, so that one pagination can see truncated or duplicated rows
-- ('RemoteConnectedUsersPaginated''s continuation is store-bound the same
-- way). No token tagging or sticky routing is added for this. Related and
-- equally accepted: a user whose copy is incomplete (a failed row delays the
-- clear until the next copier pass) is routed to Postgres by row presence
-- and may see a truncated view until the retry completes.
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
  InsertConnection self target rel qcnv -> do
    res <- connectionStoreToCassandra $ insertConnection self target rel qcnv
    _ <- Postgres.interpretConnectionStoreToPostgres $ insertConnection self target rel qcnv
    pure res
  UpdateConnectionStatus self target status -> do
    res <- connectionStoreToCassandra $ updateConnectionStatus self target status
    _ <- Postgres.interpretConnectionStoreToPostgres $ updateConnectionStatus self target status
    pure res
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
    withMigrationLocks LockShared (MilliSeconds 500) [u] $ do
      pgContacts <- Postgres.interpretConnectionStoreToPostgres $ lookupContactListWithRelation u
      cassContacts <- connectionStoreToCassandra $ lookupContactListWithRelation u
      withMigrationLocks LockShared (MilliSeconds 500) (u : map fst (pgContacts <> cassContacts)) $ do
        -- Cassandra leg: 'deleteConnections' re-enumerates contacts from
        -- Cassandra itself, so it deletes every reverse row still in
        -- Cassandra plus u's own partitions (both no-ops once u is migrated).
        connectionStoreToCassandra $ deleteConnections u
        -- Postgres-side contacts may include contacts whose Cassandra rows
        -- are already cleared (u migrated); their reverse rows must still be
        -- deleted from Cassandra.
        embed @Cas.Client $ deleteIncomingConnections u (map fst pgContacts)
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

-- | Users from the given list whose @connection@/@connection_remote@ rows are
-- present in Postgres. Presence is a statement local to this module.
filterMigrated :: (PGConstraints r) => [UserId] -> Sem r [UserId]
filterMigrated leftUsers = do
  migrated <- runStatement (Vector.map toUUID (Vector.fromList leftUsers)) selectMigratedUsers
  pure (Id <$> Vector.toList migrated)

selectMigratedUsers :: Hasql.Statement (Vector UUID) (Vector UUID)
selectMigratedUsers =
  [vectorStatement|SELECT DISTINCT ("left" :: uuid) FROM connection WHERE "left" = ANY ($1 :: uuid[])
                      UNION
                      SELECT DISTINCT ("left" :: uuid) FROM connection_remote WHERE "left" = ANY ($1 :: uuid[])
                     |]

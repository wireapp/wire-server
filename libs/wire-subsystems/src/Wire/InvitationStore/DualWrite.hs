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

module Wire.InvitationStore.DualWrite
  ( interpretInvitationStoreToCassandraAndPostgres,
  )
where

import Cassandra (ClientState)
import Data.Id (Id (..), TeamId)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.UUID (UUID)
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Polysemy.Async
import Polysemy.Conc.Effect.Race
import Polysemy.Error
import Polysemy.Resource (Resource)
import Polysemy.Time (MilliSeconds (..))
import Polysemy.TinyLog
import Wire.InvitationStore
import Wire.InvitationStore.Cassandra qualified as Cassandra
import Wire.InvitationStore.Postgres qualified as Postgres
import Wire.MigrationLock
import Wire.Postgres (PGConstraints, runStatement)

-- | During the invitations migration, reads and writes are routed per team
-- under shared migration locks while the background copier copies a team's
-- @team_invitation@ and @team_scim_pending_user_email@ partitions and deletes
-- them from Cassandra under the exclusive lock:
--
-- * Reads: the dual interpreter takes the shared per-team lock, asks Postgres
--   whether the team has already been migrated (row presence in
--   @team_invitation@ / @team_scim_pending_user_email@), and serves the team
--   from Postgres or Cassandra accordingly. This serializes reads against
--   the copier, so a team is never read from a half-deleted Cassandra
--   partition or a half-populated Postgres copy.
--
-- Accepted edge case (same as the connections migration): because inserts are
-- mirrored to Postgres for every team, row presence is only an approximation
-- of "copied"; a team whose copy is still being retried is routed to Postgres
-- and may see a truncated invitation list until the copier completes the
-- team.
--
-- * Writes: mirrored to both stores under the shared team lock. Without the
--   lock a concurrent delete could be resurrected in Postgres by this
--   interpreter or re-copied by the migrator; and an insert issued after the
--   copier deleted the Cassandra partition would otherwise land in Cassandra
--   only and be lost for migrated readers.
--
-- Accepted residual window: an invitation whose mirrored Postgres insert
-- fails after the Cassandra write succeeded is re-copied by the migrator only
-- if the Cassandra partition has not yet been cleared; once the team is
-- cleared, such a row is lost until the invitation is re-created. This is the
-- same mirroring-failure window the connections migration (WPB-22953)
-- accepted, bounded to the dual-write mode.
--
-- Mutations take shared locks per team: 'DeleteAllTeamInvitations' mutates
-- all of a team's rows, so per-invitation keys would not exclude the copier.
interpretInvitationStoreToCassandraAndPostgres ::
  forall r.
  ( PGConstraints r,
    Member TinyLog r,
    Member Async r,
    Member Race r,
    Member Resource r,
    Member (Error MigrationLockError) r
  ) =>
  ClientState ->
  InterpreterFor InvitationStore r
interpretInvitationStoreToCassandraAndPostgres casClient = interpret $ \case
  InsertInvitation inv codeTimeout ->
    withMigrationLocks LockShared (MilliSeconds 500) [inv.teamId] $ do
      inv' <- Cassandra.interpretInvitationStoreToCassandra casClient $ insertInvitation inv codeTimeout
      void $ Postgres.interpretInvitationStoreToPostgres $ insertInvitation inv codeTimeout
      pure inv'
  LookupInvitation tid iid ->
    routeSingleRead tid
      (\_ -> Postgres.interpretInvitationStoreToPostgres $ lookupInvitation tid iid)
      (\_ -> Cassandra.interpretInvitationStoreToCassandra casClient $ lookupInvitation tid iid)
  LookupInvitationByCode code -> do
    -- A code lookup can name a team that is migrated and one that is not (the
    -- code index is global); consult both stores, the migrated one first.
    withMigrationLocks LockShared (MilliSeconds 500) ([] :: [TeamId]) $ do
      pgRes <- Postgres.interpretInvitationStoreToPostgres $ lookupInvitationByCode code
      case pgRes of
        Just _ -> pure pgRes
        Nothing -> Cassandra.interpretInvitationStoreToCassandra casClient $ lookupInvitationByCode code
  LookupInvitationsByEmail email -> do
    -- The email index is global: run both stores and concatenate (a team not
    -- yet migrated can only have its rows in Cassandra). Teams whose
    -- partitions have not been cleared can exist in both stores — mirrored
    -- inserts already put them in Postgres — so deduplicate by (team,
    -- invitation id).
    withMigrationLocks LockShared (MilliSeconds 500) ([] :: [TeamId]) $ do
      pgRes <- Postgres.interpretInvitationStoreToPostgres $ lookupInvitationsByEmail email
      cassRes <- Cassandra.interpretInvitationStoreToCassandra casClient $ lookupInvitationsByEmail email
      pure $ nubBy sameInvitation (pgRes <> cassRes)
  InsertPendingScimUser tid email uid ->
    -- Lock on (team, email) via 'MigrationLockable (TeamId, EmailAddress)',
    -- whose advisory key is the team-wide "team_invitation" lock: this
    -- serializes against the copy loop's exclusive team lock so a deleted
    -- pending-SCIM row cannot be resurrected by a stale re-copy.
    withMigrationLocks LockShared (MilliSeconds 500) [(tid, email)] $ do
      Cassandra.interpretInvitationStoreToCassandra casClient $ insertPendingScimUser tid email uid
      Postgres.interpretInvitationStoreToPostgres $ insertPendingScimUser tid email uid
  LookupPendingScimUsers tid email ->
    routeSingleRead tid
      (\_ -> Postgres.interpretInvitationStoreToPostgres $ lookupPendingScimUsers tid email)
      (\_ -> Cassandra.interpretInvitationStoreToCassandra casClient $ lookupPendingScimUsers tid email)
  DeletePendingScimUser tid email uid ->
    withMigrationLocks LockShared (MilliSeconds 500) [(tid, email)] $ do
      Cassandra.interpretInvitationStoreToCassandra casClient $ deletePendingScimUser tid email uid
      Postgres.interpretInvitationStoreToPostgres $ deletePendingScimUser tid email uid
  LookupInvitationsPaginated mSize tid miid ->
    routeSingleRead tid
      (\_ -> Postgres.interpretInvitationStoreToPostgres $ lookupInvitationsPaginated mSize tid miid)
      (\_ -> Cassandra.interpretInvitationStoreToCassandra casClient $ lookupInvitationsPaginated mSize tid miid)
  CountInvitations tid ->
    routeSingleRead tid
      (\_ -> Postgres.interpretInvitationStoreToPostgres $ countInvitations tid)
      (\_ -> Cassandra.interpretInvitationStoreToCassandra casClient $ countInvitations tid)
  DeleteInvitation tid invId ->
    withMutationLock tid $ do
      Cassandra.interpretInvitationStoreToCassandra casClient $ deleteInvitation tid invId
      Postgres.interpretInvitationStoreToPostgres $ deleteInvitation tid invId
  DeleteAllTeamInvitations tid ->
    withMutationLock tid $ do
      Cassandra.interpretInvitationStoreToCassandra casClient $ deleteAllTeamInvitations tid
      Postgres.interpretInvitationStoreToPostgres $ deleteAllTeamInvitations tid
  where
    withMutationLock tid =
      withMigrationLocks LockShared (MilliSeconds 500) [tid :: TeamId]

    sameInvitation a b =
      (a.teamId, a.invitationId) == (b.teamId, b.invitationId)

-- | Serve a read whose partition is keyed by a single team: under the shared
-- lock, the whole read goes to Postgres if the team is migrated, else to
-- Cassandra.
routeSingleRead ::
  ( PGConstraints r,
    Member TinyLog r,
    Member (Error MigrationLockError) r,
    Member Async r,
    Member Race r,
    Member Resource r
  ) =>
  TeamId ->
  (() -> Sem r a) ->
  (() -> Sem r a) ->
  Sem r a
routeSingleRead team pgAction cassAction =
  withMigrationLocks LockShared (MilliSeconds 500) [team] $ do
    migrated <- filterMigrated [team]
    if null migrated then cassAction () else pgAction ()

-- | Teams from the given list whose @team_invitation@ /
-- @team_scim_pending_user_email@ rows are present in Postgres. Presence is a
-- statement local to this module.
filterMigrated :: (PGConstraints r) => [TeamId] -> Sem r [TeamId]
filterMigrated teams = do
  migrated <- runStatement (Vector.map toUUID (Vector.fromList teams)) selectMigratedTeams
  pure (Id <$> Vector.toList migrated)

selectMigratedTeams :: Hasql.Statement (Vector UUID) (Vector UUID)
selectMigratedTeams =
  [vectorStatement|SELECT DISTINCT (team :: uuid) FROM team_invitation WHERE team = ANY ($1 :: uuid[])
                      UNION
                      SELECT DISTINCT (team :: uuid) FROM team_scim_pending_user_email WHERE team = ANY ($1 :: uuid[])
                     |]

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

module Wire.MlsKeyPackageStore.Migration
  ( migrateMlsKeyPackagesLoop,
    migrateUserKeyPackages,
    lookupUserClients,
  )
where

import Cassandra
import Cassandra qualified as Cas
import Data.ByteString.Conversion
import Data.ByteString.Lazy qualified as LBS
import Data.Conduit
import Data.Conduit.List qualified as C
import Data.Id
import Hasql.Pool.Extended qualified as Hasql
import Hasql.Transaction.Sessions (IsolationLevel (ReadCommitted), Mode (Write))
import Imports hiding (lookup)
import Polysemy
import Polysemy.Async
import Polysemy.AtomicState
import Polysemy.Conc (interpretRace)
import Polysemy.Conc.Effect.Race hiding (Timeout)
import Polysemy.Input
import Polysemy.Resource (Resource, resourceToIOFinal)
import Polysemy.TinyLog
import Prometheus qualified
import System.Logger qualified as Log
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.KeyPackage
import Wire.Migration
import Wire.MlsKeyPackageStore.Postgres qualified as Postgres
import Wire.Postgres
import Wire.Sem.Logger (mapLogger)
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)
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

-- | Per-user locked migration of MLS key packages from Cassandra to
-- Postgres (same users strategy as @Wire.UserStore.Migration@ and the
-- remote-conversation-membership migration in @Wire.ConversationStore.Migration@).
--
-- Key packages are one-time-use security material — claiming deletes them —
-- so a naive copy loop can resurrect a claimed key package in Postgres, and
-- a delete-mirror could resurface one if the delete is lost or reordered.
-- Instead:
--
--   1. Per user, take an exclusive migration lock (blocking concurrent brig
--      access through 'Wire.MlsKeyPackageStore.Migrating.interpretMlsKeyPackageStoreToCassandraAndPostgres'
--      for that user), copy the user's partition into Postgres, and mark the
--      user in @mls_key_packages_migrated_users@ — all in one transaction for
--      the copy+marker.
--   2. Cassandra rows are retained during migration (users precedent): they
--      go stale for migrated users and are never consulted again, because
--      routing is keyed on the marker and the copy loop skips users already
--      marked. This makes worker restarts and error-retry passes safe: a
--      re-pass over a marked user is a no-op, so key packages already
--      claimed (i.e. deleted) in Postgres can never be resurrected from the
--      stale Cassandra partition.
--
-- The marker table is required because row presence in @mls_key_packages@
-- cannot distinguish "migrated, zero key packages" from "not yet migrated".
--
-- Ops precondition: the copy loop must only run while every brig replica is
-- in @migration-to-postgresql@ mode. A replica still serving @cassandra@
-- takes no migration locks and consults no marker, so its claims\/deletes
-- can race the copy loop and leave a consumed key package behind in
-- Postgres.
--
-- Cutover: once @wire_mls_key_packages_migration_finished@ is 1, flip
-- @postgresMigration.mlsKeyPackages@ to @postgresql@. With on-demand
-- migration ('Wire.MlsKeyPackageStore.Migrating.interpretMlsKeyPackageStoreToCassandraAndPostgres'
-- migrates an unmarked user on first access under the exclusive lock), every
-- user is marked by the time the pass finishes: the enumeration covered all
-- users in the @clients@ table, and any user brig touched since was
-- on-demand-migrated. No quiet period or reconciliation pass is needed. Do
-- not roll back to Cassandra after cutover: migrated users' claims happened
-- in Postgres, so their Cassandra copies are stale — same rollback caveat as
-- the users migration.
migrateMlsKeyPackagesLoop ::
  MigrationOptions ->
  ClientState ->
  Hasql.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  IO ()
migrateMlsKeyPackagesLoop migOpts cassClient pgPool logger migCounter migFinished migFailed migDuration =
  migrationLoop
    logger
    "mls key packages"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "mls key packages")
    (migrateAllMlsKeyPackages migOpts migCounter migDuration)

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

migrateAllMlsKeyPackages ::
  ( Member (Input Hasql.Pool) r,
    Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r,
    Member Async r,
    Member Race r,
    Member Resource r,
    Member (AtomicState Int) r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  ConduitM () Void (Sem r) ()
migrateAllMlsKeyPackages migOpts migCounter migDuration = do
  lift $ info $ Log.msg (Log.val "migrateAllMlsKeyPackages")
  withCount (paginateSem selectUserWithClients (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize runIdentity
    .| C.mapM_
      ( traverse_ $ \u ->
          handleLockAndDBErrors
            (toByteString' u)
            ( withExclusiveMigrationLockAndTimeout migOpts.timeout migDuration [u] $ do
                clients <- lookupUserClients u
                migrated <- migrateUserKeyPackages u clients
                when migrated $ liftIO (Prometheus.incCounter migCounter)
            )
      )
  where
    -- The partition key of @mls_key_packages@ is (user, client, cipher_suite),
    -- so neither a @SELECT DISTINCT user@ nor a @user = ?@ restriction alone is
    -- valid CQL on that table. Enumerate users from brig's @clients@ table
    -- instead: its partition key is @user@ alone, so a @DISTINCT user@ scan is
    -- valid CQL, and brig maintains the table on client registration\/
    -- deletion, so it covers every claimable key-package partition. Rows
    -- outside clients × cipher suites are unreachable dead data (brig's claim
    -- path iterates the user's clients × suites) and are intentionally not
    -- copied.
    selectUserWithClients :: PrepQuery R () (Identity UserId)
    selectUserWithClients = "SELECT DISTINCT user FROM clients"

-- | The claimable key-package universe is exactly the user's registered
-- clients × all cipher suites (brig stores\/claims per client × suite), so
-- copy each (client, suite) partition with a point query restricted on the
-- full partition key.
allCipherSuites :: [CipherSuiteTag]
allCipherSuites = [minBound .. maxBound]

-- | Copy an unmarked user's key packages into Postgres and mark them
-- migrated, in one copy+marker transaction. Returns 'True' iff the copy and
-- mark actually executed (the caller owns any migration counter). Safe to
-- call under contention: a user already marked is a no-op, so a concurrent
-- migration can never resurrect claimed key packages.
migrateUserKeyPackages ::
  (PGConstraints r, Member (Input ClientState) r) =>
  UserId ->
  Cas.Set ClientId ->
  Sem r Bool
migrateUserKeyPackages u clients = do
  -- Short-circuit on the marker: a user already marked migrated must never
  -- be re-copied. Claiming (and user-driven deletes) remove rows from
  -- Postgres, so re-inserting the retained stale Cassandra partition would
  -- resurrect consumed one-time-use key packages.
  migrated <- Postgres.isUserKeyPackagesMigrated u
  if migrated
    then pure False
    else do
      partitions <-
        concat
          <$> for
            (fromSet clients)
            ( \cid -> for allCipherSuites $ \suite -> do
                rows <-
                  embedClientInput $
                    retry x5 (query selectUserKeyPackages (params LocalQuorum (u, cid, suite)))
                pure
                  ( cid,
                    [ (KeyPackageRef (unKeyPackageBlob ref'), suite, KeyPackageData (unKeyPackageBlob kpData'))
                    | (ref', kpData') <- rows
                    ]
                  )
            )
      runTransactionWithRetry ReadCommitted Write $ do
        for_ partitions $ uncurry (Postgres.insertKeyPackagesTx u)
        Postgres.markUserKeyPackagesMigratedTx u
      pure True

selectUserKeyPackages :: PrepQuery R (UserId, ClientId, CipherSuiteTag) (KeyPackageBlob, KeyPackageBlob)
selectUserKeyPackages =
  "SELECT ref, data FROM mls_key_packages WHERE user = ? AND client = ? AND cipher_suite = ?"

-- | The user's registered clients from brig's Cassandra @clients@ table
-- (one row per client, partition key @user@ alone). Used by the worker's
-- per-user copy and brig's on-demand migration path.
lookupUserClients ::
  (Member (Input ClientState) r, Member (Embed IO) r) =>
  UserId ->
  Sem r (Cas.Set ClientId)
lookupUserClients u =
  Cas.Set . map runIdentity
    <$> embedClientInput (retry x5 (query selectUserClients (params LocalQuorum (Identity u))))

selectUserClients :: PrepQuery R (Identity UserId) (Identity ClientId)
selectUserClients = "SELECT client FROM clients WHERE user = ?"

-- | Cassandra blobs need a newtype: plain 'ByteString' has no 'Cql' instance.
newtype KeyPackageBlob = KeyPackageBlob {unKeyPackageBlob :: ByteString}
  deriving stock (Eq, Show)

instance Cql KeyPackageBlob where
  ctype = Tagged BlobColumn
  toCql = CqlBlob . LBS.fromStrict . unKeyPackageBlob
  fromCql (CqlBlob b) = Right . KeyPackageBlob . LBS.toStrict $ b
  fromCql _ = Left "Expected CqlBlob"

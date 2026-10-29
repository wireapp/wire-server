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
  )
where

import Cassandra
import Data.Bifunctor (first)
import Data.ByteString.Conversion
import Data.ByteString.Lazy qualified as LBS
import Data.Conduit
import Data.Conduit.List qualified as C
import Data.Id
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
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
-- @postgresMigration.mlsKeyPackages@ to @postgresql@. Note that @finished@
-- means "a full pass completed without errors", not "Cassandra is drained":
-- a user whose first key package lands in Cassandra after the enumeration
-- page has passed them is not marked in that pass (zero rows). Flip only
-- after @finished@ is 1 @and@ key package upload traffic has been quiet for
-- longer than one full pass duration, so stragglers are covered by a later
-- pass. No reconciliation pass is otherwise needed. Do not roll back to
-- Cassandra after cutover: migrated users' claims happened in Postgres, so
-- their Cassandra copies are stale — same rollback caveat as the users
-- migration.
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
  withCount (paginateSem selectUsers (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize runIdentity
    .| C.mapM_
      ( traverse_ $ \u ->
          handleLockAndDBErrors
            (toByteString' u)
            (withExclusiveMigrationLockAndTimeout migOpts.timeout migDuration [u] (migrateUserKeyPackages u migCounter))
      )
  where
    selectUsers :: PrepQuery R () (Identity UserId)
    selectUsers = "SELECT DISTINCT user FROM mls_key_packages"

migrateUserKeyPackages ::
  (PGConstraints r, Member (Input ClientState) r, Member TinyLog r) =>
  UserId ->
  Prometheus.Counter ->
  Sem r ()
migrateUserKeyPackages u migCounter = do
  -- Short-circuit on the marker: a user already marked migrated must never
  -- be re-copied. Claiming (and user-driven deletes) remove rows from
  -- Postgres, so re-inserting the retained stale Cassandra partition would
  -- resurrect consumed one-time-use key packages.
  migrated <- Postgres.isUserKeyPackagesMigrated u
  unless migrated $ do
    rows <-
      embedClientInput $
        retry x5 (query selectUserKeyPackages (params LocalQuorum (Identity u)))
    kps <-
      catMaybes <$> for rows \(c, suite, ref', kpData') ->
        case (,)
          <$> cipherSuiteTagFromInt32 suite
          <*> first Text.pack (runParser parser (encodeUtf8 c)) of
          Left parseErr -> do
            warn $
              Log.msg (Log.val "skipping mls key package with invalid client id or cipher suite")
                . Log.field "user" (toByteString' u)
                . Log.field "error" (Text.unpack parseErr)
            pure Nothing
          Right (s, cid) ->
            pure (Just (cid, s, KeyPackageRef (unKeyPackageBlob ref'), KeyPackageData (unKeyPackageBlob kpData')))
    runTransactionWithRetry ReadCommitted Write $ do
      for_ kps $ \(cid, s, ref, kpData) ->
        Postgres.insertKeyPackagesTx u cid [(ref, s, kpData)]
      Postgres.markUserKeyPackagesMigratedTx u
    liftIO $ Prometheus.incCounter migCounter

selectUserKeyPackages :: PrepQuery R (Identity UserId) (Text, Int32, KeyPackageBlob, KeyPackageBlob)
selectUserKeyPackages =
  "SELECT client, cipher_suite, ref, data FROM mls_key_packages WHERE user = ?"

-- | Cassandra blobs need a newtype: plain 'ByteString' has no 'Cql' instance.
newtype KeyPackageBlob = KeyPackageBlob {unKeyPackageBlob :: ByteString}
  deriving stock (Eq, Show)

instance Cql KeyPackageBlob where
  ctype = Tagged BlobColumn
  toCql = CqlBlob . LBS.fromStrict . unKeyPackageBlob
  fromCql (CqlBlob b) = Right . KeyPackageBlob . LBS.toStrict $ b
  fromCql _ = Left "Expected CqlBlob"

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

-- | Copy-only migration loop: no Cassandra deletes and no migration locks.
--
-- Deliberately NO dual-write (WPB-29582 review): key packages are
-- one-time-use security material — claiming deletes them — so a lost or
-- reordered delete-mirror would resurface a claimed key package in
-- Postgres after cutover. Writes stay on Cassandra until operators flip
-- @postgresMigration.mlsKeyPackages@ to @postgresql@.
--
-- Cutover requirement: the copy loop never deletes, so key packages
-- claimed in Cassandra after their row was copied remain in Postgres.
-- Therefore flip @postgresMigration.mlsKeyPackages@ to @postgresql@ first
-- (Cassandra is then frozen: post-cutover claims delete only from
-- Postgres), and immediately afterwards run a reconciliation pass deleting
-- Postgres rows absent from Cassandra — ignoring rows uploaded after the
-- flip, which exist only in Postgres. After reconciliation, disable the
-- background copy loop (@migrateMlsKeyPackages: false@): a worker restart
-- with the loop still enabled would re-copy stale Cassandra rows over
-- Postgres state.
migrateMlsKeyPackagesLoop ::
  MigrationOptions ->
  ClientState ->
  Hasql.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  IO ()
migrateMlsKeyPackagesLoop migOpts cassClient pgPool logger migCounter migFinished migFailed =
  migrationLoop
    logger
    "mls key packages"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "mls key packages")
    (migrateAllMlsKeyPackages migOpts migCounter)

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
    Member (AtomicState Int) r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  ConduitM () Void (Sem r) ()
migrateAllMlsKeyPackages migOpts migCounter = do
  lift $ info $ Log.msg (Log.val "migrateAllMlsKeyPackages")
  withCount (paginateSem selectAllKeyPackages (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| C.mapM_
      ( traverse_ $ \row@(u, c, suite, ref, _) ->
          handleErrors (toByteString' (show (u, c, suite, ref))) (migrateRow migCounter row)
      )

migrateRow ::
  (PGConstraints r, Member TinyLog r) =>
  Prometheus.Counter ->
  (UserId, Text, Int32, KeyPackageBlob, KeyPackageBlob) ->
  Sem r ()
migrateRow migCounter (u, c, suite, ref', kpData') =
  case (,)
    <$> cipherSuiteTagFromInt32 suite
    <*> first Text.pack (runParser parser (encodeUtf8 c)) of
    Left parseErr ->
      warn $
        Log.msg (Log.val "skipping mls key package with invalid client id or cipher suite")
          . Log.field "error" (Text.unpack parseErr)
    Right (s, cid) -> do
      let ref = unKeyPackageBlob ref'
          kpData = unKeyPackageBlob kpData'
      Postgres.insertKeyPackagesPg u cid [(KeyPackageRef ref, s, KeyPackageData kpData)]
      liftIO $ Prometheus.incCounter migCounter

selectAllKeyPackages :: PrepQuery R () (UserId, Text, Int32, KeyPackageBlob, KeyPackageBlob)
selectAllKeyPackages =
  "SELECT user, client, cipher_suite, ref, data FROM mls_key_packages"

-- | Cassandra blobs need a newtype: plain 'ByteString' has no 'Cql' instance.
newtype KeyPackageBlob = KeyPackageBlob {unKeyPackageBlob :: ByteString}
  deriving stock (Eq, Show)

instance Cql KeyPackageBlob where
  ctype = Tagged BlobColumn
  toCql = CqlBlob . LBS.fromStrict . unKeyPackageBlob
  fromCql (CqlBlob b) = Right . KeyPackageBlob . LBS.toStrict $ b
  fromCql _ = Left "Expected CqlBlob"

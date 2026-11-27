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

module Wire.MlsKeyPackageStore.Postgres
  ( interpretMlsKeyPackageStoreToPostgres,
  )
where

import Data.Id
import Data.Map qualified as Map
import Data.UUID (UUID)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.KeyPackage hiding (DeleteKeyPackages)
import Wire.API.PostgresMarshall
import Wire.MlsKeyPackageStore (MlsKeyPackageStore (..))
import Wire.Postgres

interpretMlsKeyPackageStoreToPostgres ::
  (PGConstraints r) =>
  InterpreterFor MlsKeyPackageStore r
interpretMlsKeyPackageStoreToPostgres = interpret $ \case
  InsertKeyPackages u c kps -> insertKeyPackagesPg u c kps
  LookupKeyPackages u c s -> toList <$> runStatement (u, c, s) lookupStatement
  LookupKeyPackagesBulk requests -> lookupKeyPackagesBulkPg requests
  DeleteKeyPackages u c s rs ->
    unless (null rs) $
      runStatement (u, c, s, Vector.fromList rs) deleteKeyPackagesStatement
  DeleteAllKeyPackages u c ss ->
    unless (null ss) $
      runStatement (u, c, Vector.fromList (toList ss)) deleteAllKeyPackagesStatement
  ClaimKeyPackage u c s candidates
    | null candidates -> pure Nothing
    | otherwise -> runStatement (u, c, s, Vector.fromList candidates) claimKeyPackageStatement

insertKeyPackagesPg :: (PGConstraints r) => UserId -> ClientId -> [(KeyPackageRef, CipherSuiteTag, KeyPackageData)] -> Sem r ()
insertKeyPackagesPg _ _ [] = pure ()
insertKeyPackagesPg u c kps =
  runStatement
    ( u,
      c,
      Vector.fromList [s | (_, s, _) <- kps],
      Vector.fromList [r | (r, _, _) <- kps],
      Vector.fromList [d | (_, _, d) <- kps]
    )
    insertStatement

-- | Bulk insert. A key package ref is a hash of the cipher suite and the key
-- package data, so a conflicting row already holds the same data and
-- @DO NOTHING@ is sufficient (it also tolerates duplicates within one batch).
insertStatement :: Hasql.Statement (UserId, ClientId, Vector CipherSuiteTag, Vector KeyPackageRef, Vector KeyPackageData) ()
insertStatement =
  lmapPG
    ( [resultlessStatement|INSERT INTO mls_key_packages (user_id, client, cipher_suite, ref, data)
                           SELECT $1 :: uuid, $2 :: text, s, r, d
                           FROM unnest ($3 :: int4[], $4 :: bytea[], $5 :: bytea[]) AS t(s, r, d)
                           ON CONFLICT (user_id, client, cipher_suite, ref) DO NOTHING
                          |] ::
        Hasql.Statement (UUID, Text, Vector Int32, Vector ByteString, Vector ByteString) ()
    )

-- | Atomically remove and return one random key package among the given
-- candidate refs. Rows that are locked by a concurrent claim are skipped, so
-- two concurrent claims (possibly on different brig instances) never hand out
-- the same key package.
claimKeyPackageStatement :: Hasql.Statement (UserId, ClientId, CipherSuiteTag, Vector KeyPackageRef) (Maybe (KeyPackageRef, KeyPackageData))
claimKeyPackageStatement =
  dimapPG
    ( [maybeStatement|DELETE FROM mls_key_packages
                      WHERE user_id = $1 :: uuid AND client = $2 :: text AND cipher_suite = $3 :: int4
                        AND ref = (SELECT ref FROM mls_key_packages
                                   WHERE user_id = $1 :: uuid AND client = $2 :: text AND cipher_suite = $3 :: int4
                                     AND ref = ANY ($4 :: bytea[])
                                   ORDER BY random()
                                   LIMIT 1
                                   FOR UPDATE SKIP LOCKED)
                      RETURNING ref :: bytea, data :: bytea
                     |] ::
        Hasql.Statement (UUID, Text, Int32, Vector ByteString) (Maybe (ByteString, ByteString))
    )

lookupStatement :: Hasql.Statement (UserId, ClientId, CipherSuiteTag) (Vector (KeyPackageRef, KeyPackageData))
lookupStatement =
  dimapPG
    [vectorStatement|SELECT ref :: bytea, data :: bytea
                     FROM mls_key_packages
                     WHERE user_id = $1 :: uuid AND client = $2 :: text AND cipher_suite = $3 :: int4
                    |]

-- | Look up key packages for many (user, client, cipher suite) triples with a
-- single unnest-join query. Keys without matching rows are absent from the
-- result (same contract as the Cassandra interpreter).
lookupKeyPackagesBulkPg ::
  (PGConstraints r) =>
  [(UserId, ClientId, CipherSuiteTag)] ->
  Sem r (Map.Map (UserId, ClientId, CipherSuiteTag) [(KeyPackageRef, KeyPackageData)])
lookupKeyPackagesBulkPg requests =
  Map.fromListWith (<>) . map (\(u, c, s, ref, dat) -> ((u, c, s), [(ref, dat)])) . toList
    <$> runStatement (dedupedRequests, dedupedClients, dedupedSuites) lookupBulkStatement
  where
    triples = Map.toAscList (Map.fromList [(r, ()) | r <- requests])
    dedupedRequests = Vector.fromList [u | ((u, _, _), _) <- triples]
    dedupedClients = Vector.fromList [c | ((_, c, _), _) <- triples]
    dedupedSuites = Vector.fromList [s | ((_, _, s), _) <- triples]

lookupBulkStatement ::
  Hasql.Statement
    (Vector UserId, Vector ClientId, Vector CipherSuiteTag)
    [(UserId, ClientId, CipherSuiteTag, KeyPackageRef, KeyPackageData)]
lookupBulkStatement =
  dimapPG
    ( [vectorStatement|SELECT k.user_id :: uuid, k.client :: text, k.cipher_suite :: int4, k.ref :: bytea, k.data :: bytea
                       FROM mls_key_packages k
                       JOIN unnest ($1 :: uuid[], $2 :: text[], $3 :: int4[]) AS q(u, c, s)
                         ON k.user_id = q.u AND k.client = q.c AND k.cipher_suite = q.s
                      |] ::
        Hasql.Statement
          (Vector UUID, Vector Text, Vector Int32)
          (Vector (UUID, Text, Int32, ByteString, ByteString))
    )

deleteKeyPackagesStatement :: Hasql.Statement (UserId, ClientId, CipherSuiteTag, Vector KeyPackageRef) ()
deleteKeyPackagesStatement =
  lmapPG
    ( [resultlessStatement|DELETE FROM mls_key_packages
                           WHERE user_id = $1 :: uuid AND client = $2 :: text AND cipher_suite = $3 :: int4 AND ref = ANY ($4 :: bytea[])
                          |] ::
        Hasql.Statement (UUID, Text, Int32, Vector ByteString) ()
    )

deleteAllKeyPackagesStatement :: Hasql.Statement (UserId, ClientId, Vector CipherSuiteTag) ()
deleteAllKeyPackagesStatement =
  lmapPG
    ( [resultlessStatement|DELETE FROM mls_key_packages
                           WHERE user_id = $1 :: uuid AND client = $2 :: text AND cipher_suite = ANY ($3 :: int4[])
                          |] ::
        Hasql.Statement (UUID, Text, Vector Int32) ()
    )

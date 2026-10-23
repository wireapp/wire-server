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

-- | During migration Cassandra remains the source of truth: all reads go to
-- Cassandra, all writes are applied to Cassandra first and then mirrored to
-- Postgres.
module Wire.ClientStore.DualWrite
  ( interpretClientStoreToCassandraAndPostgres,
  )
where

import Imports
import Polysemy
import Polysemy.TinyLog
import Wire.API.User.Client.Prekey
import Wire.ClientStore
import Wire.ClientStore.Cassandra
import Wire.ClientStore.Postgres qualified as Postgres
import Wire.Postgres (PGConstraints)
import Wire.Sem.Metrics (Metrics)

interpretClientStoreToCassandraAndPostgres ::
  ( Member TinyLog r,
    Member (Final IO) r,
    Member Metrics r,
    PGConstraints r
  ) =>
  ClientStoreCassandraEnv ->
  InterpreterFor ClientStore r
interpretClientStoreToCassandraAndPostgres env = interpret $ \case
  -- Reads: Cassandra only.
  LookupClient uid cid -> cas $ lookupClient uid cid
  LookupClients uid -> cas $ lookupClients uid
  LookupClientIds uid -> cas $ lookupClientIds uid
  LookupClientIdsBulk uids -> cas $ lookupClientIdsBulk uids
  LookupClientsBulk uids -> cas $ lookupClientsBulk uids
  LookupPubClientsBulk uids -> cas $ lookupPubClientsBulk uids
  LookupPrekeyIds uid cid -> cas $ lookupPrekeyIds uid cid
  LookupPrekeyPresenceBulk pairs -> cas $ lookupPrekeyPresenceBulk pairs
  GetActivityTimestamps uid -> cas $ getActivityTimestamps uid
  LookupMLSPublicKey uid cid scheme -> cas $ lookupMLSPublicKey uid cid scheme
  -- Writes: Cassandra (source of truth) first, then mirrored to Postgres.
  Upsert uid cid timestamp nc -> do
    result <- cas $ upsert uid cid timestamp nc
    _ <- Postgres.interpretClientStoreToPostgres $ upsert uid cid timestamp nc
    pure result
  Delete uid cid -> do
    cas $ delete uid cid
    Postgres.interpretClientStoreToPostgres $ delete uid cid
  UpdateLabel uid cid lbl -> do
    cas $ updateLabel uid cid lbl
    Postgres.interpretClientStoreToPostgres $ updateLabel uid cid lbl
  UpdateCapabilities uid cid caps -> do
    cas $ updateCapabilities uid cid caps
    Postgres.interpretClientStoreToPostgres $ updateCapabilities uid cid caps
  UpdateLastActive uid cid timestamp -> do
    cas $ updateLastActive uid cid timestamp
    Postgres.interpretClientStoreToPostgres $ updateLastActive uid cid timestamp
  UpdatePrekeys uid cid prekeys -> do
    cas $ updatePrekeys uid cid prekeys
    Postgres.interpretClientStoreToPostgres $ updatePrekeys uid cid prekeys
  AddMLSPublicKeys uid cid keys -> do
    result <- cas $ addMLSPublicKeys uid cid keys
    _ <- Postgres.interpretClientStoreToPostgres $ addMLSPublicKeys uid cid keys
    pure result
  -- The Cassandra claim consumes the prekey from the source of truth; the
  -- claimed key (unless it is the never-consumed last-resort key) is deleted
  -- in Postgres to keep the mirror consistent.
  ClaimPrekey uid cid -> do
    result <- cas $ claimPrekey uid cid
    when (fmap (prekeyId . prekeyData) result /= Just lastPrekeyId) $
      traverse_ (\k -> Postgres.deletePrekeyImpl uid cid (prekeyId (prekeyData k))) result
    pure result
  where
    cas ::
      ( Member TinyLog r,
        Member (Final IO) r,
        Member Metrics r
      ) =>
      Sem (ClientStore ': r) a ->
      Sem r a
    cas = interpretClientStoreCassandra env

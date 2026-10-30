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

module Wire.VerificationCodeStore.DualWrite
  ( interpretVerificationCodeStoreToCassandraAndPostgres,
  )
where

import Cassandra (ClientState)
import Imports
import Polysemy
import Wire.Postgres (PGConstraints)
import Wire.VerificationCodeStore
import Wire.VerificationCodeStore.Cassandra qualified as Cassandra
import Wire.VerificationCodeStore.Postgres qualified as Postgres

-- | Cassandra is the source of truth during migration; writes are mirrored to
-- Postgres (Cassandra first, mirroring PR #5417's DualWrite ordering).
interpretVerificationCodeStoreToCassandraAndPostgres ::
  (PGConstraints r) =>
  ClientState ->
  InterpreterFor VerificationCodeStore r
interpretVerificationCodeStoreToCassandraAndPostgres casClient = interpret $ \case
  InsertCode c -> do
    Cassandra.interpretVerificationCodeStoreCassandra casClient $ insertCode c
    Postgres.interpretVerificationCodeStoreToPostgres $ insertCode c
  LookupCode key scope ->
    Cassandra.interpretVerificationCodeStoreCassandra casClient $ lookupCode key scope
  DeleteCode key scope -> do
    Cassandra.interpretVerificationCodeStoreCassandra casClient $ deleteCode key scope
    Postgres.interpretVerificationCodeStoreToPostgres $ do
      Postgres.insertCodeDeletionMarker key scope
      deleteCode key scope
  InsertThrottle key scope t -> do
    Cassandra.interpretVerificationCodeStoreCassandra casClient $ insertThrottle key scope t
    Postgres.interpretVerificationCodeStoreToPostgres $ insertThrottle key scope t
  LookupThrottle key scope ->
    Cassandra.interpretVerificationCodeStoreCassandra casClient $ lookupThrottle key scope

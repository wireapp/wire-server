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

module Wire.DomainVerificationChallengeStore.DualWrite
  ( interpretDomainVerificationChallengeStoreToCassandraAndPostgres,
  )
where

import Cassandra
import Imports
import Polysemy
import Polysemy.Input
import Util.Timeout
import Wire.DomainVerificationChallengeStore
import Wire.DomainVerificationChallengeStore qualified as DomainVerificationChallengeStore
import Wire.DomainVerificationChallengeStore.Cassandra qualified as Cassandra
import Wire.DomainVerificationChallengeStore.Postgres qualified as Postgres
import Wire.Postgres

-- | Cassandra is the source of truth during migration; writes are mirrored to Postgres.
interpretDomainVerificationChallengeStoreToCassandraAndPostgres ::
  ( Member (Input ClientState) r,
    PGConstraints r
  ) =>
  Timeout ->
  InterpreterFor DomainVerificationChallengeStore r
interpretDomainVerificationChallengeStoreToCassandraAndPostgres to = interpret $ \case
  Insert challenge -> do
    Cassandra.interpretDomainVerificationChallengeStoreToCassandra to $ DomainVerificationChallengeStore.insert challenge
    Postgres.interpretDomainVerificationChallengeStoreToPostgres to $ DomainVerificationChallengeStore.insert challenge
  Lookup challengeId ->
    Cassandra.interpretDomainVerificationChallengeStoreToCassandra to $ DomainVerificationChallengeStore.lookup challengeId
  Delete challengeId -> do
    Cassandra.interpretDomainVerificationChallengeStoreToCassandra to $ DomainVerificationChallengeStore.delete challengeId
    Postgres.interpretDomainVerificationChallengeStoreToPostgres to $ DomainVerificationChallengeStore.delete challengeId

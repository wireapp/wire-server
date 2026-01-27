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

module Wire.CodeStore.DualWrite
  ( interpretCodeStoreToCassandraAndPostgres,
  )
where

import Cassandra (ClientState)
import Data.Misc
import Imports
import Polysemy
import Polysemy.Input
import Wire.CodeStore (CodeStore (..))
import Wire.CodeStore qualified as CodeStore
import Wire.CodeStore.Cassandra qualified as Cassandra
import Wire.CodeStore.Postgres qualified as Postgres
import Wire.Postgres (PGConstraints)

-- | Cassandra is the sourceof truth during migration; writes are mirrored to Postgres.
interpretCodeStoreToCassandraAndPostgres ::
  ( Member (Input ClientState) r,
    Member (Input (Either HttpsUrl (Map Text HttpsUrl))) r,
    PGConstraints r
  ) =>
  Sem (CodeStore ': r) a ->
  Sem r a
interpretCodeStoreToCassandraAndPostgres = interpret $ \case
  GetCode k -> do
    Cassandra.interpretCodeStoreToCassandra $ CodeStore.getCode k
  CreateCode code mPw -> do
    Cassandra.interpretCodeStoreToCassandra $ CodeStore.createCode code mPw
    Postgres.interpretCodeStoreToPostgres $ CodeStore.createCode code mPw
  DeleteCode k -> do
    Cassandra.interpretCodeStoreToCassandra $ CodeStore.deleteCode k
    Postgres.interpretCodeStoreToPostgres $ CodeStore.deleteCode k
  MakeKey cid -> do
    Cassandra.interpretCodeStoreToCassandra $ CodeStore.makeKey cid
  GenerateCode cid t -> do
    Cassandra.interpretCodeStoreToCassandra $ CodeStore.generateCode cid t
  GetConversationCodeURI mbHost -> do
    Cassandra.interpretCodeStoreToCassandra $ CodeStore.getConversationCodeURI mbHost

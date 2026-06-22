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
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.CodeStore (CodeStore (..))
import Wire.CodeStore qualified as CodeStore
import Wire.CodeStore.Cassandra qualified as Cassandra
import Wire.CodeStore.Code (CodeReferent (..), codeReferent)
import Wire.CodeStore.Postgres qualified as Postgres
import Wire.Postgres (PGConstraints)

-- | Cassandra is the source of truth during migration; writes are mirrored to Postgres.
interpretCodeStoreToCassandraAndPostgres ::
  ( Member (Input ClientState) r,
    Member (Input (Either HttpsUrl (Map Text HttpsUrl))) r,
    Member (ErrorS 'CodeStoreNotFound) r,
    PGConstraints r
  ) =>
  Sem (CodeStore ': r) a ->
  Sem r a
interpretCodeStoreToCassandraAndPostgres = interpret $ \case
  GetCode k -> do
    Cassandra.interpretCodeStoreToCassandra $ CodeStore.getCode k
  CreateCode code mPw -> do
    case codeReferent code of
      CodeReferentConv _ -> Cassandra.interpretCodeStoreToCassandra $ CodeStore.createCode code mPw
      CodeReferentMeeting _ -> pure ()
    Postgres.interpretCodeStoreToPostgres $ CodeStore.createCode code mPw
  DeleteCode k -> do
    Cassandra.interpretCodeStoreToCassandra $ CodeStore.deleteCode k
    Postgres.interpretCodeStoreToPostgres $ CodeStore.deleteCode k
  MakeKey ref -> case ref of
    CodeReferentConv _ -> Cassandra.interpretCodeStoreToCassandra $ CodeStore.makeKey ref
    CodeReferentMeeting _ -> Postgres.interpretCodeStoreToPostgres $ CodeStore.makeKey ref
  GenerateCode ref t -> case ref of
    CodeReferentConv _ -> Cassandra.interpretCodeStoreToCassandra $ CodeStore.generateCode ref t
    CodeReferentMeeting _ -> Postgres.interpretCodeStoreToPostgres $ CodeStore.generateCode ref t
  GetConversationCodeURI mbHost -> do
    Cassandra.interpretCodeStoreToCassandra $ CodeStore.getConversationCodeURI mbHost

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

module Wire.ScimTokenStore.DualWrite
  ( interpretScimTokenStoreToCassandraAndPostgres,
  )
where

import Cassandra (Client)
import Imports
import Polysemy
import Wire.API.User.Scim (ScimTokenLookupKey (..))
import Wire.Postgres (PGConstraints)
import Wire.ScimTokenStore
import Wire.ScimTokenStore.Cassandra qualified as Cassandra
import Wire.ScimTokenStore.Postgres qualified as Postgres

-- | Cassandra is the source of truth during migration; writes are mirrored to
-- Postgres. The one exception is 'Lookup': the plaintext-to-hashed re-keying
-- that happens on read is applied to BOTH stores, so legacy plaintext tokens
-- get mirrored into Postgres under their hashed key.
interpretScimTokenStoreToCassandraAndPostgres ::
  ( Member (Embed Client) r,
    PGConstraints r
  ) =>
  InterpreterFor ScimTokenStore r
interpretScimTokenStoreToCassandraAndPostgres = interpret $ \case
  Insert st sti -> do
    Cassandra.scimTokenStoreToCassandra (insert st sti)
    Postgres.interpretScimTokenStoreToPostgres (insert st sti)
  Lookup st -> do
    mRow <- embed @Client $ Cassandra.lookupScimTokenRaw st
    case mRow of
      Just (ScimTokenLookupKeyPlaintext plain, info) -> do
        embed @Client $ Cassandra.convertPlaintextToken plain info
        Postgres.pgConvertPlaintextToken plain info
        pure (Just info)
      Just (ScimTokenLookupKeyHashed _, info) -> pure (Just info)
      Nothing -> pure Nothing
  LookupByTeam tid ->
    Cassandra.scimTokenStoreToCassandra (lookupByTeam tid)
  UpdateName team tid name -> do
    Cassandra.scimTokenStoreToCassandra (updateName team tid name)
    Postgres.interpretScimTokenStoreToPostgres (updateName team tid name)
  Delete team tid -> do
    Cassandra.scimTokenStoreToCassandra (delete team tid)
    Postgres.interpretScimTokenStoreToPostgres (delete team tid)
  DeleteByTeam team -> do
    Cassandra.scimTokenStoreToCassandra (deleteByTeam team)
    Postgres.interpretScimTokenStoreToPostgres (deleteByTeam team)

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

module Wire.SAMLUserStore.DualWrite
  ( interpretSAMLUserStoreToCassandraAndPostgres,
  )
where

import Cassandra (Client)
import Imports
import Polysemy
import Wire.Postgres (PGConstraints)
import Wire.SAMLUserStore
import Wire.SAMLUserStore.Cassandra qualified as Cassandra
import Wire.SAMLUserStore.Postgres qualified as Postgres

-- | Cassandra is the source of truth during migration: all reads (including
-- the full listing) stay on Cassandra. 'Get' mirrors the Cassandra
-- legacy-@user@ re-keying into Postgres (like the plaintext-token re-keying in
-- "Wire.ScimTokenStore.DualWrite"), so a user whose row only exists in the
-- legacy table is not lost at cutover; writes are mirrored to Postgres.
interpretSAMLUserStoreToCassandraAndPostgres ::
  ( Member (Embed Client) r,
    PGConstraints r
  ) =>
  InterpreterFor SAMLUserStore r
interpretSAMLUserStoreToCassandraAndPostgres = interpret $ \case
  Insert ur uid -> do
    Cassandra.samlUserStoreToCassandra (insert ur uid)
    Postgres.interpretSAMLUserStoreToPostgres (insert ur uid)
  Get ur -> do
    mNew <- embed @Client $ Cassandra.getSAMLUserRaw ur
    case mNew of
      Just uid -> pure (Just uid)
      Nothing -> do
        -- legacy migrate-on-read: re-key into BOTH stores
        mLegacy <- embed @Client $ Cassandra.lookupSAMLUserLegacy ur
        traverse
          ( \uid -> do
              embed @Client $ Cassandra.insertSAMLUser ur uid
              Postgres.interpretSAMLUserStoreToPostgres (insert ur uid)
              pure uid
          )
          mLegacy
  DeleteByIssuer iss -> do
    Cassandra.samlUserStoreToCassandra (deleteByIssuer iss)
    Postgres.interpretSAMLUserStoreToPostgres (deleteByIssuer iss)
  Delete uid ur -> do
    Cassandra.samlUserStoreToCassandra (delete uid ur)
    Postgres.interpretSAMLUserStoreToPostgres (delete uid ur)
  GetAllByIssuer iss ->
    Cassandra.samlUserStoreToCassandra (getAllByIssuer iss)

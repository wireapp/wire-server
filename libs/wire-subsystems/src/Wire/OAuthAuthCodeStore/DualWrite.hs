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

module Wire.OAuthAuthCodeStore.DualWrite
  ( interpretOAuthAuthCodeStoreToCassandraAndPostgres,
  )
where

import Cassandra (MonadClient)
import Imports
import Polysemy
import Wire.OAuthAuthCodeStore
import Wire.OAuthAuthCodeStore.Cassandra qualified as Cassandra
import Wire.OAuthAuthCodeStore.Postgres qualified as Postgres
import Wire.Postgres (PGConstraints)

-- | Cassandra is the source of truth during migration: lookups are served from
-- Cassandra. Writes go to both stores. Because codes are consumed on read, a
-- successful lookup deletes the code from BOTH stores — a consumed code must
-- not remain queryable in Postgres.
interpretOAuthAuthCodeStoreToCassandraAndPostgres ::
  forall m r.
  ( MonadClient m,
    Member (Embed m) r,
    PGConstraints r
  ) =>
  InterpreterFor OAuthAuthCodeStore r
interpretOAuthAuthCodeStoreToCassandraAndPostgres = interpret $ \case
  InsertOAuthAuthCode ttl code cid uid scope uri chal -> do
    Cassandra.oauthAuthCodeStoreToCassandra @m $
      insertOAuthAuthCode ttl code cid uid scope uri chal
    Postgres.interpretOAuthAuthCodeStoreToPostgres $
      insertOAuthAuthCode ttl code cid uid scope uri chal
  LookupAndDeleteOAuthAuthCode code -> do
    result <-
      Cassandra.oauthAuthCodeStoreToCassandra @m $
        lookupAndDeleteOAuthAuthCode code
    -- The Postgres delete runs unconditionally: it is a single idempotent
    -- DELETE ... RETURNING, and a miss does NOT guarantee the row is absent
    -- there (a prior consume whose Postgres half failed can leave a zombie
    -- until its expiry).
    void $ Postgres.interpretOAuthAuthCodeStoreToPostgres $ lookupAndDeleteOAuthAuthCode code
    pure result

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

module Wire.PushTokenStore.DualWrite
  ( interpretPushTokenStoreToCassandraAndPostgres,
  )
where

import Cassandra (ClientState)
import Imports
import Polysemy
import Polysemy.Input
import Wire.Postgres (PGConstraints)
import Wire.PushTokenStore
import Wire.PushTokenStore qualified as Store
import Wire.PushTokenStore.Cassandra qualified as Cassandra
import Wire.PushTokenStore.Postgres qualified as Postgres

-- | Cassandra is the source of truth during migration; writes are mirrored
-- to Postgres, reads still come from Cassandra.
interpretPushTokenStoreToCassandraAndPostgres ::
  ( Member (Input ClientState) r,
    PGConstraints r
  ) =>
  Sem (PushTokenStore ': r) a ->
  Sem r a
interpretPushTokenStoreToCassandraAndPostgres = interpret $ \case
  LookupPushTokens u c ->
    runCassandra $ Store.lookupPushTokens u c
  InsertPushToken u t a p arn conn client -> do
    runCassandra $ Store.insertPushToken u t a p arn conn client
    Postgres.interpretPushTokenStoreToPostgres $ Store.insertPushToken u t a p arn conn client
  UpdatePushTokenArn u t a p arn -> do
    runCassandra $ Store.updatePushTokenArn u t a p arn
    Postgres.interpretPushTokenStoreToPostgres $ Store.updatePushTokenArn u t a p arn
  DeletePushToken u t a p -> do
    runCassandra $ Store.deletePushToken u t a p
    Postgres.interpretPushTokenStoreToPostgres $ Store.deletePushToken u t a p
  ErasePushTokens u -> do
    runCassandra $ Store.erasePushTokens u
    Postgres.interpretPushTokenStoreToPostgres $ Store.erasePushTokens u

-- | Runs the Cassandra interpreter with the client state taken from the
-- ambient 'Input' effect.
runCassandra ::
  ( Member (Input ClientState) r,
    Member (Embed IO) r
  ) =>
  Sem (PushTokenStore ': r) a ->
  Sem r a
runCassandra = Cassandra.interpretPushTokenStoreToCassandra

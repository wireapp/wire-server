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

module Wire.MLSCommitLockStore.DualWrite
  ( interpretMLSCommitLockStoreToCassandraAndPostgres,
  )
where

import Cassandra (ClientState)
import Imports
import Polysemy
import Polysemy.TinyLog (TinyLog)
import Wire.ConversationStore (LockAcquired (..), MLSCommitLockStore (..))
import Wire.ConversationStore qualified as CommitLockStore
import Wire.ConversationStore.Cassandra qualified as Cassandra
import Wire.MLSCommitLockStore.Postgres qualified as Postgres
import Wire.Postgres (PGConstraints)

-- | During migration Cassandra stays the source of truth: every write is
-- mirrored to Postgres, and 'AcquireCommitLock' returns the Cassandra result
-- (the arbiter) so mutual exclusion is governed by a single store until the
-- cutover to 'PostgresqlStorage'.
interpretMLSCommitLockStoreToCassandraAndPostgres ::
  ( Member TinyLog r,
    PGConstraints r
  ) =>
  ClientState ->
  InterpreterFor MLSCommitLockStore r
interpretMLSCommitLockStoreToCassandraAndPostgres client = interpret $ \case
  AcquireCommitLock gId epoch ttl -> do
    -- Cassandra is the arbiter: mirror the acquire to Postgres only when it
    -- succeeds, so Postgres never holds a lock Cassandra did not grant.
    acquired <- Cassandra.interpretMLSCommitLockStoreToCassandra client $ CommitLockStore.acquireCommitLock gId epoch ttl
    when (acquired == Acquired) $
      void $
        Postgres.interpretMLSCommitLockStoreToPostgres $
          CommitLockStore.acquireCommitLock gId epoch ttl
    pure acquired
  ReleaseCommitLock gId epoch -> do
    Cassandra.interpretMLSCommitLockStoreToCassandra client $ CommitLockStore.releaseCommitLock gId epoch
    Postgres.interpretMLSCommitLockStoreToPostgres $ CommitLockStore.releaseCommitLock gId epoch

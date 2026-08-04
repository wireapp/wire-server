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

module Wire.MLSCommitLockStore.Postgres
  ( interpretMLSCommitLockStoreToPostgres,
  )
where

import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Wire.API.MLS.Epoch (Epoch)
import Wire.API.MLS.Group (GroupId)
import Wire.API.PostgresMarshall (dimapPG, lmapPG)
import Wire.ConversationStore (LockAcquired (..), MLSCommitLockStore (..))
import Wire.Postgres (PGConstraints, runStatement)

-- | Postgres interpreter for 'MLSCommitLockStore'.
--
-- Acquire replicates Cassandra's @INSERT ... IF NOT EXISTS USING TTL@ as an
-- @INSERT ... ON CONFLICT DO UPDATE ... WHERE expires_at < now() RETURNING@:
--
-- * no existing row            -> INSERT succeeds           -> 'Acquired'
-- * existing row, still live   -> WHERE is false, no return -> 'NotAcquired'
-- * existing row, expired      -> UPDATE succeeds           -> 'Acquired'
--
-- The last case is essential: unlike Cassandra (which purges expired TTL rows),
-- Postgres keeps the dead row, so we must treat an expired lock as re-acquirable
-- or a crashed holder would block its @(group_id, epoch)@ forever.
--
-- Unlike Cassandra, Postgres never auto-purges expired rows, but the expired
-- branch above /reuses/ the existing row in place (UPDATE rather than INSERT),
-- so a re-acquired @(group_id, epoch)@ does not accumulate a second row.
-- Successful commits delete their row on release; only commits whose holder
-- crashed before release leave a dead row, which is unaddressable by future
-- commits (epochs are monotonic) and self-expires via @expires_at@. If dead-row
-- growth ever becomes operationally significant, a periodic
-- @DELETE FROM mls_commit_locks WHERE expires_at < now()@ (plus an index on
-- @expires_at@) can be added.
interpretMLSCommitLockStoreToPostgres ::
  (PGConstraints r) =>
  InterpreterFor MLSCommitLockStore r
interpretMLSCommitLockStoreToPostgres = interpret $ \case
  AcquireCommitLock gId epoch ttl -> do
    let ttlSecs = round ttl :: Int32
    acquired <- runStatement (gId, epoch, ttlSecs) acquireStmt
    pure $ maybe NotAcquired (const Acquired) acquired
  ReleaseCommitLock gId epoch ->
    runStatement (gId, epoch) releaseStmt

acquireStmt :: Hasql.Statement (GroupId, Epoch, Int32) (Maybe Bool)
acquireStmt =
  dimapPG
    [maybeStatement|
      INSERT INTO mls_commit_locks (group_id, epoch, expires_at)
      VALUES ($1 :: bytea, $2 :: int8, now() + make_interval(secs => $3 :: int4))
      ON CONFLICT (group_id, epoch) DO UPDATE
        SET expires_at = excluded.expires_at
        WHERE mls_commit_locks.expires_at < now()
      RETURNING true :: bool
    |]

releaseStmt :: Hasql.Statement (GroupId, Epoch) ()
releaseStmt =
  lmapPG
    [resultlessStatement|
      DELETE FROM mls_commit_locks
      WHERE group_id = ($1 :: bytea) AND epoch = ($2 :: int8)
    |]

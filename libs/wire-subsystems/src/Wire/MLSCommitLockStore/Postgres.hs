{-# OPTIONS_GHC -Wno-orphans #-}

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

import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Hex (hex)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Hasql.Pool qualified as Hasql
import Hasql.Pool.Extended qualified as HasqlPoolExt
import Imports
import Polysemy
import Polysemy.Error (Error)
import Polysemy.Resource (Resource)
import Polysemy.TinyLog (TinyLog)
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.ConversationStore (MLSCommitLockStore (..))
import Wire.MigrationLock

-- | Postgres interpreter for 'MLSCommitLockStore'.
--
-- Implements the lock as a session-scoped pg advisory lock (via
-- 'tryWithMigrationLock') on a connection from the given lock pool, held for
-- the duration of the action and released on completion -- or when the
-- holding connection is closed, which replaces the Cassandra 10-minute TTL as
-- the crash guard. Contention returns 'Nothing' immediately so callers respond
-- with @stale-message@ without waiting.
--
-- The lock pool must be separate from the pool used by the action (see
-- 'tryWithMigrationLock'); its size bounds the number of concurrent commits
-- per process.
--
-- Accepted trade-offs (inherent to advisory locks): there is no TTL, so a
-- hung-but-alive holder blocks the group until its connection drops; locks
-- are not replicated and vanish on a Postgres failover; and the key is a
-- hashed @Int64@, so a hash collision yields a spurious stale response.
interpretMLSCommitLockStoreToPostgres ::
  ( Member (Embed IO) r,
    Member (Error Hasql.UsageError) r,
    Member Resource r,
    Member TinyLog r
  ) =>
  HasqlPoolExt.Pool ->
  InterpreterFor MLSCommitLockStore r
interpretMLSCommitLockStoreToPostgres lockPool = interpretH $ \case
  HoldCommitLock gId epoch action -> do
    m <- runT action
    let run_it = raise . interpretMLSCommitLockStoreToPostgres lockPool
    r <- run_it $
      tryWithMigrationLock lockPool (gId, epoch) $ do
        fa <- m
        pure (Just <$> fa)
    case r of
      Just x -> pure x
      Nothing -> pureT Nothing

-- | Lock key for a group at an epoch: a stable (SHA-256 based) hash of the
-- group id and the epoch number.
instance MigrationLockable (GroupId, Epoch) where
  lockScope = "mls_commit_lock"
  lockKey (gId, epoch) =
    stableHash64 $
      unGroupId gId
        <> LBS.toStrict (Builder.toLazyByteString (Builder.word64BE (epochNumber epoch)))
  toText (gId, epoch) =
    "0x"
      <> decodeUtf8 (hex (unGroupId gId))
      <> ":"
      <> Text.pack (show (epochNumber epoch))

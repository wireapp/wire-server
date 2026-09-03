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

import Data.Bits (rotateL, xor)
import Data.Hashable (hash)
import Data.Hex (hex)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Imports
import Polysemy
import Polysemy.Resource (Resource)
import Polysemy.TinyLog (TinyLog)
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.ConversationStore (MLSCommitLockStore (..))
import Wire.MigrationLock
import Wire.Postgres (PGConstraints)

-- | Postgres interpreter for 'MLSCommitLockStore'.
--
-- Implements the lock as a session-scoped pg advisory lock (via
-- 'tryWithMigrationLock') on a dedicated pooled connection, held for the
-- duration of the action and released on completion — or when the holding
-- connection dies, which replaces the Cassandra 10-minute TTL as the crash
-- guard. Contention returns 'Nothing' immediately so callers respond with
-- @stale-message@ without waiting.
--
-- Accepted trade-offs (inherent to advisory locks): there is no TTL, so a
-- hung-but-alive holder blocks the group until its connection drops; locks
-- are not replicated and vanish on a Postgres failover; and the key is a
-- hashed @Int64@, so a hash collision yields a spurious stale response (the
-- same approach accepted for the existing @(TeamId, Text)@ instance).
interpretMLSCommitLockStoreToPostgres ::
  ( PGConstraints r,
    Member Resource r,
    Member TinyLog r
  ) =>
  InterpreterFor MLSCommitLockStore r
interpretMLSCommitLockStoreToPostgres = interpretH $ \case
  HoldCommitLock gId epoch action -> do
    m <- runT action
    let run_it = raise . interpretMLSCommitLockStoreToPostgres
    r <- run_it $
      tryWithMigrationLock (gId, epoch) $ do
        fa <- m
        pure (Just <$> fa)
    case r of
      Just x -> pure x
      Nothing -> pureT Nothing

-- | Combines group id and epoch into one lock key; rotate+xor mixes the two
-- hashes to reduce collisions.
instance MigrationLockable (GroupId, Epoch) where
  lockScope = "mls_commit_lock"
  lockKey (gId, epoch) =
    (fromIntegral (hash (unGroupId gId)) :: Int64) `rotateL` 31
      `xor` fromIntegral (epochNumber epoch)
  toText (gId, epoch) =
    "0x"
      <> decodeUtf8 (hex (unGroupId gId))
      <> ":"
      <> Text.pack (show (epochNumber epoch))

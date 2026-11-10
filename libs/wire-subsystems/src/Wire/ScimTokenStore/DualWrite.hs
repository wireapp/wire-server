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
import Polysemy.Async (Async)
import Polysemy.Conc (Race)
import Polysemy.Error (Error)
import Polysemy.Resource (Resource)
import Polysemy.TinyLog (TinyLog)
import Polysemy.Time.Data.TimeUnit (MilliSeconds (..))
import Wire.API.User.Scim (ScimTokenInfo (stiId), ScimTokenLookupKey (..))
import Wire.MigrationLock
import Wire.Postgres (PGConstraints)
import Wire.ScimTokenStore
import Wire.ScimTokenStore.Cassandra qualified as Cassandra
import Wire.ScimTokenStore.Postgres qualified as Postgres

-- | Cassandra is the source of truth during migration; writes are mirrored to
-- Postgres. The one exception is 'Lookup': the plaintext-to-hashed re-keying
-- that happens on read is applied to BOTH stores, so legacy plaintext tokens
-- get mirrored into Postgres under their hashed key.
--
-- Deletes and renames take the shared (team, token-id) migration lock and
-- complete their Cassandra write and Postgres mirror while holding it, so they
-- are serialized against the background copier, which re-reads the row from
-- Cassandra and copies it under the exclusive lock on the same key
-- ('Wire.ScimTokenStore.Migration'). Without the lock, a copier running from a
-- stale scanned page could resurrect a revoked token in Postgres — a revoked
-- SCIM bearer credential that keeps authenticating is a security incident
-- (unlike a stale copied row, it would not self-heal, because the Cassandra
-- source row is gone and nothing else would overwrite the Postgres copy).
--
-- 'Insert' is lock-free: it creates a brand-new token with a fresh id that the
-- copier never has in its scanned page, and the copier's in-lock re-read
-- cannot resurrect a row that a concurrent delete removed.
--
-- Accepted residual window (no tombstones, analogous to the connections
-- migration): a token created concurrently with 'DeleteByTeam' may keep its
-- Postgres row while its Cassandra row is removed by the team-wide delete —
-- the token was created after 'DeleteByTeam' enumerated the team's tokens, so
-- it is not among the locked keys. Fully closing this would require locking
-- the whole team partition, which the per-row locking scheme does not have.
interpretScimTokenStoreToCassandraAndPostgres ::
  ( Member (Embed Client) r,
    PGConstraints r,
    Member TinyLog r,
    Member (Error MigrationLockError) r,
    Member Async r,
    Member Race r,
    Member Resource r
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
        Postgres.pgConvertPlaintextToken plain
        pure (Just info)
      Just (ScimTokenLookupKeyHashed _, info) -> pure (Just info)
      Nothing -> pure Nothing
  LookupByTeam tid ->
    Cassandra.scimTokenStoreToCassandra (lookupByTeam tid)
  UpdateName team tid name ->
    withMigrationLocks LockShared (MilliSeconds 500) [ScimTokenLockKey (team, tid)] $ do
      Cassandra.scimTokenStoreToCassandra (updateName team tid name)
      Postgres.interpretScimTokenStoreToPostgres (updateName team tid name)
  Delete team tid ->
    withMigrationLocks LockShared (MilliSeconds 500) [ScimTokenLockKey (team, tid)] $ do
      Cassandra.scimTokenStoreToCassandra (delete team tid)
      Postgres.interpretScimTokenStoreToPostgres (delete team tid)
  DeleteByTeam team -> do
    -- Enumerate the team's tokens (Cassandra is the source of truth) and lock
    -- every one of them, so the team-wide delete is serialized against the
    -- copier row by row. Tokens created during enumeration fall under the
    -- accepted residual window documented above.
    tokens <- Cassandra.scimTokenStoreToCassandra (lookupByTeam team)
    let lockKeys = (\sti -> ScimTokenLockKey (team, sti.stiId)) <$> tokens
    withMigrationLocks LockShared (MilliSeconds 500) lockKeys $ do
      Cassandra.scimTokenStoreToCassandra (deleteByTeam team)
      Postgres.interpretScimTokenStoreToPostgres (deleteByTeam team)

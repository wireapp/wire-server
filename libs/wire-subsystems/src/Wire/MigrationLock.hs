-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

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

module Wire.MigrationLock where

import Control.Concurrent.Async qualified as Async
import Control.Exception (onException, throwIO)
import Control.Monad.Except (catchError)
import Crypto.Hash (SHA256 (..), hashWith)
import Data.Bits
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Hashable (hash)
import Data.Id
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Hasql.Pool qualified as Hasql
import Hasql.Pool.Extended qualified as HasqlPoolExt
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Network.HTTP.Types.Status (status500)
import Network.Wai.Utilities.Error qualified as Wai
import Network.Wai.Utilities.JSONResponse
import Polysemy
import Polysemy.Async
import Polysemy.Conc.Effect.Race
import Polysemy.Error
import Polysemy.Input
import Polysemy.Resource (Resource, bracket)
import Polysemy.Time.Data.TimeUnit
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as TinyLog
import System.Logger.Message qualified as Log
import System.Timeout qualified
import Wire.API.Error
import Wire.API.PostgresMarshall
import Wire.Error
import Wire.Postgres

class MigrationLockable a where
  -- | namespace (e.g. "conv", "user", etc.), used for logging only
  lockScope :: ByteString

  -- | key used for advisory locks; should be collision-resistant (unique with high probability)
  lockKey :: a -> Int64

  toText :: a -> Text

data LockType
  = -- | Used for migrating a set of data, will block any other locks
    LockExclusive
  | -- | Used for reading and writing to Cassandra, will block exclusive locks
    LockShared

data MigrationLockError = TimedOutAcquiringLock
  deriving (Show)

instance APIError MigrationLockError where
  toResponse = waiErrorToJSONResponse . migrationLockErrorToWai

instance Exception MigrationLockError

migrationLockErrorToHttpError :: MigrationLockError -> HttpError
migrationLockErrorToHttpError = StdError . migrationLockErrorToWai

migrationLockErrorToWai :: MigrationLockError -> Wai.Error
migrationLockErrorToWai _ = Wai.mkError status500 "internal-server-error" "Internal Server Error"

withMigrationLocks ::
  forall x a u r.
  ( PGConstraints r,
    Member Async r,
    Member TinyLog r,
    Member Race r,
    Member Resource r,
    Member (Error MigrationLockError) r,
    TimeUnit u,
    MigrationLockable x
  ) =>
  LockType ->
  u ->
  [x] ->
  Sem r a ->
  Sem r a
withMigrationLocks lockType maxWait lockables action = do
  bracket acquire release (const action)
  where
    acquire = do
      lockAcquired <- embed newEmptyMVar
      actionCompleted <- embed newEmptyMVar

      pool <- (.rawPool) <$> input @HasqlPoolExt.Pool
      lockThread <- async . embed . Hasql.use pool $ do
        -- Sort lockIds to avoid deadlocks
        let lockIds = sort $ fmap lockKey lockables
        Session.statement lockIds acquireLocks

        liftIO $ putMVar lockAcquired ()
        liftIO $ takeMVar actionCompleted

        Session.statement lockIds releaseLocks

      void . timeout (cancel lockThread >> throw TimedOutAcquiringLock) maxWait $ embed (takeMVar lockAcquired)
      pure (actionCompleted, lockThread)

    release (actionCompleted, lockThread) = do
      let logFirstLock =
            case lockables of
              [] -> id
              (x : _) -> Log.field ("first_" <> lockScope @x) (lockKey x)
          logError errorStr =
            TinyLog.warn $
              Log.msg (Log.val "Failed to cleanly unlock the migration locks")
                . logFirstLock
                . Log.field "numberOfLocks" (length lockables)
                . Log.field "error" errorStr
      _ <- embed $ tryPutMVar actionCompleted ()
      mEithErr <- timeout (cancel lockThread) (Seconds 1) $ await lockThread
      case mEithErr of
        Left () -> logError "timed out waiting for unlock"
        Right (Nothing) -> logError "lock/unlock thread didn't finish"
        Right (Just (Left e)) -> logError (show e)
        Right (Just (Right ())) -> pure ()

    acquireLocks :: Hasql.Statement [Int64] ()
    acquireLocks =
      lmapPG @(Vector _)
        case lockType of
          LockExclusive ->
            [resultlessStatement|SELECT (1 :: int)
                                 FROM (SELECT pg_advisory_lock(lockId)
                                       FROM (SELECT UNNEST($1 :: bigint[]) as lockId) AS t) AS t2|]
          LockShared ->
            [resultlessStatement|SELECT (1 :: int)
                                 FROM (SELECT pg_advisory_lock_shared(lockId)
                                       FROM (SELECT UNNEST($1 :: bigint[]) as lockId) AS t) AS t2|]

    releaseLocks :: Hasql.Statement [Int64] ()
    releaseLocks =
      lmapPG @(Vector _)
        case lockType of
          LockExclusive ->
            [resultlessStatement|SELECT (1 :: int)
                                 FROM (SELECT pg_advisory_unlock(lockId)
                                       FROM (SELECT UNNEST($1 :: bigint[]) as lockId) AS t) AS t2|]
          LockShared ->
            [resultlessStatement|SELECT (1 :: int)
                                 FROM (SELECT pg_advisory_unlock_shared(lockId)
                                       FROM (SELECT UNNEST($1 :: bigint[]) as lockId) AS t) AS t2|]

-- | Non-blocking, single-lock variant of 'withMigrationLocks': tries to take a
-- session-scoped advisory lock on a connection from the given lock pool, runs
-- the action while holding it and releases it afterwards. Returns 'Nothing'
-- without running the action if the key is already locked. This is a
-- try-lock (instead of the blocking 'withMigrationLocks') because instant
-- 'Nothing' preserves the existing not-acquired -> stale-message client
-- behavior.
--
-- The lock pool must be dedicated to lock holding and must not be the pool the
-- action itself uses: the lock-holding connection sits idle for the whole
-- action, so with a shared pool concurrent lock holders could exhaust it and
-- starve their own work.
--
-- The lock is taken in the (masked) acquire step of 'bracket', so an
-- asynchronous exception can't separate a taken lock from its release, and it
-- is released when the action finishes, also when it short-circuits via other
-- effects (e.g. an error response). The lock cannot outlive the holding
-- connection's checkout from the pool: if unlocking fails or the holding
-- session is interrupted, the session ends with an exception and the pool
-- destroys (closes) the connection instead of returning it, which makes
-- Postgres drop all advisory locks held by it.
--
-- The advisory-lock key is 'scopedLockKey', so equal 'lockKey's of different
-- 'MigrationLockable' scopes don't share a lock.
tryWithMigrationLock ::
  forall x a r.
  ( Member (Embed IO) r,
    Member (Error Hasql.UsageError) r,
    Member Resource r,
    Member TinyLog r,
    MigrationLockable x
  ) =>
  HasqlPoolExt.Pool ->
  x ->
  Sem r a ->
  Sem r (Maybe a)
tryWithMigrationLock lockPool lockable action =
  bracket
    (embed (tryAcquireMigrationLock lockPool (scopedLockKey lockable)) >>= either throw pure)
    (traverse_ (releaseMigrationLock lockable))
    (traverse (const action))

-- | Handle to the thread whose pooled connection holds an acquired lock.
data MigrationLockToken = MigrationLockToken
  { actionCompleted :: MVar (),
    lockThread :: Async.Async (Either Hasql.UsageError ())
  }

-- | Thrown inside the lock-holding session when unlocking fails, so that the
-- pool destroys the connection (and with it the lock) instead of reusing it.
data MigrationLockReleaseFailed = MigrationLockReleaseFailed
  deriving (Show)

instance Exception MigrationLockReleaseFailed

-- | Non-blocking acquire of a session-scoped advisory lock on a connection
-- from the pool. Returns @Right Nothing@ if the key is already locked; the
-- connection is then returned to the pool without holding any lock.
tryAcquireMigrationLock :: HasqlPoolExt.Pool -> Int64 -> IO (Either Hasql.UsageError (Maybe MigrationLockToken))
tryAcquireMigrationLock lockPool key = do
  lockAcquired <- newEmptyTMVarIO
  actionCompleted <- newEmptyMVar
  let holdLock = do
        ok <- Session.statement key tryAcquireLock
        liftIO . atomically $ putTMVar lockAcquired ok
        when ok $ do
          liftIO $ takeMVar actionCompleted
          released <- Session.statement key releaseLock `catchError` const (pure False)
          unless released . liftIO $ throwIO MigrationLockReleaseFailed
  lockThread <- Async.async $ useObserved lockPool holdLock
  -- Wait until the lock is taken (or not), or the thread ended without
  -- getting that far (pool or connection error). If we are interrupted while
  -- waiting, cancelling the thread interrupts its session, which makes the
  -- pool destroy the connection and thereby drop any lock it took.
  outcome <-
    atomically
      ( (Right <$> readTMVar lockAcquired)
          <|> (Left <$> Async.waitCatchSTM lockThread)
      )
      `onException` Async.cancel lockThread
  case outcome of
    Right True -> pure . Right . Just $ MigrationLockToken {actionCompleted, lockThread}
    Right False -> pure (Right Nothing)
    Left (Right (Left e)) -> pure (Left e)
    Left (Right (Right ())) -> pure (Right Nothing)
    Left (Left e) -> throwIO e

-- | Release a lock acquired with 'tryAcquireMigrationLock'. Signals the
-- holding session to unlock and gives it ~1s to finish cleanly; otherwise it
-- is cancelled, which makes the pool destroy its connection and thereby drop
-- the lock.
releaseMigrationLock ::
  forall x r.
  ( Member (Embed IO) r,
    Member TinyLog r,
    MigrationLockable x
  ) =>
  x ->
  MigrationLockToken ->
  Sem r ()
releaseMigrationLock lockable token = do
  let logError :: String -> Sem r ()
      logError errorStr =
        TinyLog.warn $
          Log.msg (Log.val "Failed to cleanly unlock the migration lock; its connection was discarded")
            . Log.field ("scope_" <> lockScope @x) (toText lockable)
            . Log.field "error" errorStr
  res <- embed $ do
    void $ tryPutMVar token.actionCompleted ()
    r <- System.Timeout.timeout 1_000_000 (Async.waitCatch token.lockThread)
    when (isNothing r) $ Async.cancel token.lockThread
    pure r
  case res of
    Nothing -> logError "timed out waiting for unlock"
    Just (Left e) -> logError (displayException e)
    Just (Right (Left e)) -> logError (show e)
    Just (Right (Right ())) -> pure ()

tryAcquireLock :: Hasql.Statement Int64 Bool
tryAcquireLock =
  [singletonStatement|SELECT (pg_try_advisory_lock($1 :: bigint) :: bool)|]

releaseLock :: Hasql.Statement Int64 Bool
releaseLock =
  [singletonStatement|SELECT (pg_advisory_unlock($1 :: bigint) :: bool)|]

-- | Advisory-lock key derived from both 'lockScope' and 'lockKey', so equal
-- 'lockKey's of different scopes map to different locks. Uses SHA-256 (via
-- 'stableHash64') rather than 'hash', so the key is the same on all pods
-- regardless of the @hashable@ version they were built with.
scopedLockKey :: forall x. (MigrationLockable x) => x -> Int64
scopedLockKey x =
  stableHash64 $
    lockScope @x
      <> "\0"
      <> LBS.toStrict (Builder.toLazyByteString (Builder.int64BE (lockKey x)))

-- | First 64 bits of the SHA-256 digest, as a (big-endian) 'Int64'.
stableHash64 :: ByteString -> Int64
stableHash64 bytes =
  let digest :: ByteString
      digest = BA.convert (hashWith SHA256 bytes)
   in foldl' (\acc w -> (acc `shiftL` 8) .|. fromIntegral w) 0 (BS.unpack (BS.take 8 digest))

--------------------------------------------------------------------------------
-- Combines team id and feature name into one lock key to keep per-feature locks distinct within a team
-- without introducing a separate lock table; rotate+xor mixes the two hashes to reduce collisions.
instance MigrationLockable (TeamId, Text) where
  lockKey (team, featureName) =
    let teamHash = hashUUID team
        featureHash = fromIntegral (hash featureName)
     in teamHash `xor` rotateL featureHash 1
  lockScope = "team_feature"
  toText (tid, feat) = idToText tid <> ":" <> feat

instance MigrationLockable ConvId where
  lockKey = hashUUID
  lockScope = "conv"
  toText = idToText

instance MigrationLockable UserId where
  lockKey = hashUUID
  lockScope = "user"
  toText = idToText

hashUUID :: Id a -> Int64
hashUUID (toUUID -> uuid) =
  let (w1, w2) = UUID.toWords64 uuid
      mixed = w1 `xor` (w2 `shiftR` 32) `xor` (w2 `shiftL` 32)
   in fromIntegral mixed

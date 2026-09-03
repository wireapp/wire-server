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

import Control.Exception (onException)
import Control.Concurrent.Async qualified as Async
import Data.Bits
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
                                       FROM (SELECT UNNEST($1 :: bigint[]) as lockId))|]
          LockShared ->
            [resultlessStatement|SELECT (1 :: int)
                                 FROM (SELECT pg_advisory_lock_shared(lockId)
                                       FROM (SELECT UNNEST($1 :: bigint[]) as lockId))|]

    releaseLocks :: Hasql.Statement [Int64] ()
    releaseLocks =
      lmapPG @(Vector _)
        case lockType of
          LockExclusive ->
            [resultlessStatement|SELECT (1 :: int)
                                 FROM (SELECT pg_advisory_unlock(lockId)
                                       FROM (SELECT UNNEST($1 :: bigint[]) as lockId))|]
          LockShared ->
            [resultlessStatement|SELECT (1 :: int)
                                 FROM (SELECT pg_advisory_unlock_shared(lockId)
                                       FROM (SELECT UNNEST($1 :: bigint[]) as lockId))|]

-- | Non-blocking variant of 'withMigrationLocks' for a single lock: acquires a
-- session-scoped advisory lock for the key on a dedicated pooled connection,
-- runs the action, releases. Returns 'Nothing' without running the action if
-- the key is already locked. This is a try-lock (instead of the blocking
-- 'withMigrationLocks') because instant 'Nothing' preserves the existing
-- not-acquired -> stale-message client behavior.
--
-- The release is bracketed with 'Polysemy.Resource.bracket', so it also runs
-- when the action short-circuits via other effects (e.g. an error response).
tryWithMigrationLock ::
  forall x a r.
  ( PGConstraints r,
    Member Resource r,
    Member TinyLog r,
    MigrationLockable x
  ) =>
  x ->
  Sem r a ->
  Sem r (Maybe a)
tryWithMigrationLock lockable action =
  tryAcquireMigrationLock lockable >>= \case
    Nothing -> pure Nothing
    Just token ->
      Just
        <$> bracket
          (pure ())
          (const (releaseMigrationLock lockable token))
          (const action)

-- | Opaque handle to the connection holding an acquired advisory lock.
data MigrationLockToken = MigrationLockToken
  { actionCompleted :: MVar (),
    lockThread :: Async.Async ()
  }

-- | Non-blocking acquire of a session-scoped advisory lock for the key on a
-- dedicated pooled connection. Returns 'Nothing' without side effects if the
-- key is already locked.
tryAcquireMigrationLock ::
  forall x r.
  ( PGConstraints r,
    MigrationLockable x
  ) =>
  x ->
  Sem r (Maybe MigrationLockToken)
tryAcquireMigrationLock lockable = do
  lockAcquired <- embed newEmptyMVar
  actionCompleted <- embed newEmptyMVar

  pool <- (.rawPool) <$> input @HasqlPoolExt.Pool
  lockThread <-
    embed . Async.async $
      let holdSession =
            Hasql.use pool $ do
              ok <- Session.statement (lockKey lockable) tryAcquireLock
              liftIO $ putMVar lockAcquired (Right ok)
              when ok $ do
                liftIO $ takeMVar actionCompleted
                Session.statement [lockKey lockable] releaseLock
          -- If the session failed before signaling (e.g. connection error),
          -- the caller would otherwise block forever on 'lockAcquired'.
          signalFailure = \case
            Left e -> void (tryPutMVar lockAcquired (Left e))
            Right _ -> pure ()
       in holdSession >>= signalFailure

  -- Cancelling the thread ends its session, which releases any advisory lock
  -- it may already have taken; without this an async exception while waiting
  -- here would leak the lock.
  acquired <- embed $ takeMVar lockAcquired `onException` Async.cancel lockThread
  case acquired of
    Left e -> do
      embed $ Async.cancel lockThread
      throw e
    Right False -> do
      embed $ Async.cancel lockThread
      pure Nothing
    Right True ->
      pure . Just $ MigrationLockToken {actionCompleted, lockThread}

-- | Release a lock acquired with 'tryAcquireMigrationLock'. Signals the
-- holding connection to unlock and gives it ~1s to finish cleanly.
releaseMigrationLock ::
  forall x r.
  ( PGConstraints r,
    Member TinyLog r,
    MigrationLockable x
  ) =>
  x ->
  MigrationLockToken ->
  Sem r ()
releaseMigrationLock lockable token = do
  let MigrationLockToken {actionCompleted, lockThread} = token
      logError errorStr =
        TinyLog.warn $
          Log.msg (Log.val "Failed to cleanly unlock the migration locks")
            . Log.field ("scope_" <> lockScope @x) (lockKey lockable)
            . Log.field "error" errorStr
  _ <- embed $ tryPutMVar actionCompleted ()
  mRes <- embed $ System.Timeout.timeout 1_000_000 (Async.wait lockThread)
  case mRes of
    Nothing -> logError ("timed out waiting for unlock" :: Text)
    Just () -> pure ()

tryAcquireLock :: Hasql.Statement Int64 Bool
tryAcquireLock =
  [singletonStatement|SELECT (pg_try_advisory_lock($1 :: bigint) :: bool)|]

releaseLock :: Hasql.Statement [Int64] ()
releaseLock =
  lmapPG @(Vector _)
    [resultlessStatement|SELECT (1 :: int)
                         FROM (SELECT pg_advisory_unlock(lockId)
                               FROM (SELECT UNNEST($1 :: bigint[]) as lockId))|]

--------------------------------------------------------------------------------
-- INSTANCES

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

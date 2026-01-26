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

module Wire.MigrationLock where

import Data.Proxy
import Data.Vector (Vector)
import Hasql.Pool qualified as Hasql
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Polysemy.Async
import Polysemy.Conc.Effect.Race
import Polysemy.Error
import Polysemy.Input
import Polysemy.Time.Data.TimeUnit
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as TinyLog
import System.Logger.Message qualified as Log
import Wire.API.PostgresMarshall
import Wire.Postgres

class MigrationLockable a where
  -- | namespace (e.g. "conv", "user", etc.), used for logging only
  lockScope :: proxy a -> ByteString

  -- | globally unique key
  lockKey :: a -> Int64

data LockType
  = -- | Used for migrating a set of data, will block any other locks
    LockExclusive
  | -- | Used for reading and writing to Cassandra, will block exclusive locks
    LockShared

data MigrationLockError = TimedOutAcquiringLock
  deriving (Show)

withMigrationLocks ::
  forall x a u r.
  ( PGConstraints r,
    Member Async r,
    Member TinyLog r,
    Member Race r,
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
  lockAcquired <- embed newEmptyMVar
  actionCompleted <- embed newEmptyMVar

  pool <- input
  lockThread <- async . embed . Hasql.use pool $ do
    let lockIds = fmap lockKey lockables
    Session.statement lockIds acquireLocks

    liftIO $ putMVar lockAcquired ()
    liftIO $ takeMVar actionCompleted

    Session.statement lockIds releaseLocks

  void . timeout (cancel lockThread >> throw TimedOutAcquiringLock) maxWait $ embed (takeMVar lockAcquired)
  res <- action
  embed $ putMVar actionCompleted ()

  mEithErr <- timeout (cancel lockThread) (Seconds 1) $ await lockThread
  let logFirstLock =
        case lockables of
          [] -> id
          (x : _) -> Log.field ("first_" <> lockScope (Proxy @x)) (lockKey x)
      logError errorStr =
        TinyLog.warn $
          Log.msg (Log.val "Failed to cleanly unlock the migration locks")
            . logFirstLock
            . Log.field "numberOfLocks" (length lockables)
            . Log.field "error" errorStr
  case mEithErr of
    Left () -> logError "timed out waiting for unlock"
    Right (Nothing) -> logError "lock/unlock thread didn't finish"
    Right (Just (Left e)) -> logError (show e)
    Right (Just (Right ())) -> pure ()

  pure res
  where
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

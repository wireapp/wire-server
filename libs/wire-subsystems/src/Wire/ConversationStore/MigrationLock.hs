module Wire.ConversationStore.MigrationLock where

import Data.Bits
import Data.Id
import Data.UUID qualified as UUID
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

data LockType
  = -- | Used for migrating a conversation, will block any other locks
    LockExclusive
  | -- | Used for reading and writing to Cassandra, will block exclusive locks
    LockShared

data MigrationLockError = TimedOutAcquiringLock
  deriving (Show)

withMigrationLocks ::
  ( PGConstraints r,
    Member Async r,
    Member TinyLog r,
    Member Race r,
    Member (Error MigrationLockError) r,
    TimeUnit u
  ) =>
  LockType ->
  u ->
  [Either ConvId UserId] ->
  Sem r a ->
  Sem r a
withMigrationLocks lockType maxWait convOrUsers action = do
  lockAcquired <- embed newEmptyMVar
  actionCompleted <- embed newEmptyMVar

  pool <- input
  lockThread <- async . embed . Hasql.use pool $ do
    let lockIds = map mkLockId convOrUsers
    Session.statement lockIds acquireLocks

    liftIO $ putMVar lockAcquired ()
    liftIO $ takeMVar actionCompleted

    Session.statement lockIds releaseLocks

  void . timeout (cancel lockThread >> throw TimedOutAcquiringLock) maxWait $ embed (takeMVar lockAcquired)
  res <- action
  embed $ putMVar actionCompleted ()

  mEithErr <- timeout (cancel lockThread) (Seconds 1) $ await lockThread
  let logFirstLock =
        case convOrUsers of
          [] -> id
          (convOrUser : _) -> Log.field (either (const "first_conv") (const "first_user") convOrUser) (either idToText idToText convOrUser)
      logError errorStr =
        TinyLog.warn $
          Log.msg (Log.val "Failed to cleanly unlock the migration locks")
            . logFirstLock
            . Log.field "numberOfLocks" (length convOrUsers)
            . Log.field "error" errorStr
  case mEithErr of
    Left () -> logError "timed out waiting for unlock"
    Right (Nothing) -> logError "lock/unlock thread didn't finish"
    Right (Just (Left e)) -> logError (show e)
    Right (Just (Right ())) -> pure ()

  pure res
  where
    mkLockId :: Either ConvId UserId -> Int64
    mkLockId convOrUser = fromIntegral $ case convOrUser of
      Left convId -> hashUUID convId
      Right userId -> hashUUID userId

    hashUUID :: Id a -> Int64
    hashUUID (toUUID -> uuid) =
      let (w1, w2) = UUID.toWords64 uuid
          mixed = w1 `xor` (w2 `shiftR` 32) `xor` (w2 `shiftL` 32)
       in fromIntegral mixed

    acquireLocks :: Hasql.Statement [Int64] ()
    acquireLocks =
      lmapPG @[_] @(Vector _)
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
      lmapPG @[_] @(Vector _)
        case lockType of
          LockExclusive ->
            [resultlessStatement|SELECT (1 :: int)
                                 FROM (SELECT pg_advisory_unlock(lockId)
                                       FROM (SELECT UNNEST($1 :: bigint[]) as lockId))|]
          LockShared ->
            [resultlessStatement|SELECT (1 :: int)
                                 FROM (SELECT pg_advisory_unlock_shared(lockId)
                                       FROM (SELECT UNNEST($1 :: bigint[]) as lockId))|]

module Wire.ConversationStore.MigrationLock where

import Data.Bits
import Data.Id
import Data.UUID qualified as UUID
import Hasql.Pool qualified as Hasql
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Polysemy.Async
import Polysemy.Input
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as TinyLog
import System.Logger.Message qualified as Log
import Wire.ConversationStore.Postgres

data LockType
  = -- | Used for migrating a conversation, will block any other locks
    LockExclusive
  | -- | Used for reading and writing to Cassandra, will block exclusive locks
    LockShared

withMigrationLock ::
  ( PGConstraints r,
    Member Async r,
    Member TinyLog r
  ) =>
  LockType ->
  Either ConvId UserId ->
  Sem r a ->
  Sem r a
withMigrationLock lockType convOrUser action = do
  lockAcquired <- embed newEmptyMVar
  actionCompleted <- embed newEmptyMVar

  pool <- input
  lockThread <- async . embed . Hasql.use pool $ do
    _ <- Session.statement lockId acquireLock
    liftIO $ putMVar lockAcquired ()
    liftIO $ takeMVar actionCompleted
    Session.statement lockId releaseLock

  -- TODO: We should time this out and log in case the lock is taken by another
  -- process which gets stuck
  embed $ takeMVar lockAcquired
  res <- action
  embed $ putMVar actionCompleted ()

  -- TODO: Do we need a timeout here?
  mEithErr <- await lockThread
  case mEithErr of
    Just (Right ()) -> pure ()
    Just (Left e) ->
      TinyLog.warn $
        Log.msg (Log.val "Failed to cleanly unlock the migration lock")
          . Log.field (either (const "conv") (const "user") convOrUser) (either idToText idToText convOrUser)
          . Log.field "error" (show e)
    Nothing ->
      TinyLog.warn $
        Log.msg (Log.val "Failed to cleanly unlock the migration lock")
          . Log.field (either (const "conv") (const "user") convOrUser) (either idToText idToText convOrUser)
          . Log.field "error" ("N/A" :: ByteString)
  pure res
  where
    lockId :: Int64
    lockId = fromIntegral $ case convOrUser of
      Left convId -> hashUUID convId
      Right userId -> hashUUID userId

    hashUUID :: Id a -> Int64
    hashUUID (toUUID -> uuid) =
      let (w1, w2) = UUID.toWords64 uuid
          mixed = w1 `xor` (w2 `shiftR` 32) `xor` (w2 `shiftL` 32)
       in fromIntegral mixed

    acquireLock :: Hasql.Statement (Int64) ()
    acquireLock =
      case lockType of
        LockExclusive -> [resultlessStatement|SELECT (1 :: int) FROM (SELECT pg_advisory_lock($1 :: bigint))|]
        LockShared -> [resultlessStatement|SELECT (1 :: int) FROM (SELECT pg_advisory_lock_shared($1 :: bigint))|]

    releaseLock :: Hasql.Statement (Int64) ()
    releaseLock =
      case lockType of
        LockExclusive -> [resultlessStatement|SELECT (1 :: int) FROM (SELECT pg_advisory_unlock($1 :: bigint))|]
        LockShared -> [resultlessStatement|SELECT (1 :: int) FROM (SELECT pg_advisory_unlock_shared($1 :: bigint))|]

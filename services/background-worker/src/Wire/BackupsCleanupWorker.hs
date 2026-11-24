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

module Wire.BackupsCleanupWorker
  ( backupsCleanupMaxCalls,
    cleanupBackupsLoop,
    guardedCleanup,
    runBackupsCleanup,
  )
where

import Data.Id (RequestId (RequestId))
import Data.Text qualified as T
import Imports
import Polysemy
import System.Logger qualified as Log
import UnliftIO (tryAny)
import Wire.API.Backup (BackupCleanupResult (..))
import Wire.BackgroundWorker.Env (AppT, Env (..))
import Wire.CargoholdAPIAccess
import Wire.Effects (runBackgroundWorkerEffects)
import Wire.ExternalAccess.External (ExtEnv)

-- | Upper bound of cleanup calls (batches) per run.
backupsCleanupMaxCalls :: Int
backupsCleanupMaxCalls = 20

-- | Calls the cleanup until a call deletes nothing, at most the given number of times.
-- Returns the total number of deleted files and the number of calls.
cleanupBackupsLoop :: (Member CargoholdAPIAccess r) => Int -> Sem r (Word32, Int)
cleanupBackupsLoop maxCalls = go 0 0
  where
    go total calls
      | calls >= maxCalls = pure (total, calls)
      | otherwise = do
          BackupCleanupResult deleted <- cleanupBackups
          if deleted == 0
            then pure (total, calls + 1)
            else go (total + deleted) (calls + 1)

-- | Runs the loop and keeps count of the completed calls. A synchronous exception (the Rpc
-- layer throws connection and non-2xx failures as IO exceptions) or an error of the runner
-- ends the run: 'Left' holds the failure and the progress so far. Async exceptions are not
-- caught.
guardedCleanup ::
  (Member CargoholdAPIAccess r, Member (Embed IO) r) =>
  (Sem r (Word32, Int) -> IO (Either Text (Word32, Int))) ->
  Int ->
  IO (Either (Text, (Word32, Int)) (Word32, Int))
guardedCleanup runner maxCalls = do
  progress <- newIORef (0, 0)
  let counted = intercept @CargoholdAPIAccess $ \CleanupBackups -> do
        result@(BackupCleanupResult n) <- cleanupBackups
        embed @IO $ modifyIORef' progress (\(total, calls) -> (total + n, calls + 1))
        pure result
  outcome <- tryAny . runner . counted $ cleanupBackupsLoop maxCalls
  soFar <- readIORef progress
  pure $ case outcome of
    Left e -> Left (T.pack (displayException e), soFar)
    Right (Left err) -> Left (err, soFar)
    Right (Right done) -> Right done

-- | One run of the cron job. A failing call ends the run, the next tick retries.
runBackupsCleanup :: ExtEnv -> AppT IO ()
runBackupsCleanup extEnv = do
  env <- ask
  result <-
    liftIO . guardedCleanup (runBackgroundWorkerEffects env extEnv (RequestId "backups-cleanup") Nothing) $
      backupsCleanupMaxCalls
  case result of
    Left (err, (total, calls)) ->
      Log.err env.logger $
        Log.msg (Log.val "Backups cleanup failed")
          . Log.field "error" err
          . Log.field "total_deleted" total
          . Log.field "calls" calls
    Right (total, calls) -> do
      let report =
            Log.msg (Log.val "Backups cleanup finished")
              . Log.field "total_deleted" total
              . Log.field "calls" calls
      if total > 0 then Log.info env.logger report else Log.debug env.logger report

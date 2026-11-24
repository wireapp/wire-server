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
    runBackupsCleanup,
  )
where

import Data.Id (RequestId (RequestId))
import Imports
import Polysemy
import System.Logger qualified as Log
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

-- | One run of the cron job. A failing call ends the run, the next tick retries.
runBackupsCleanup :: ExtEnv -> AppT IO ()
runBackupsCleanup extEnv = do
  env <- ask
  result <-
    liftIO
      . runBackgroundWorkerEffects env extEnv (RequestId "backups-cleanup") Nothing
      $ cleanupBackupsLoop backupsCleanupMaxCalls
  case result of
    Left err ->
      Log.err env.logger $
        Log.msg (Log.val "Backups cleanup failed")
          . Log.field "error" err
    Right (total, calls) ->
      Log.info env.logger $
        Log.msg (Log.val "Backups cleanup finished")
          . Log.field "total_deleted" total
          . Log.field "calls" calls

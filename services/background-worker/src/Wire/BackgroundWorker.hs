module Wire.BackgroundWorker where

import Imports
import qualified Wire.BackendNotificationPusher as BackendNotificationPusher
import Wire.BackgroundWorker.Env
import Wire.BackgroundWorker.Options

-- TODO: Start an http service with status and metrics endpoints
run :: Opts -> IO ()
run opts = do
  env <- mkEnv opts
  -- FUTUREWORK: Make some way to tracking all the workers, currently there is
  -- only one so we can just block on it.
  BackendNotificationPusher.startWorker env opts.remoteDomains

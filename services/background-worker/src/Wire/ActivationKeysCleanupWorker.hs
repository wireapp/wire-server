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

module Wire.ActivationKeysCleanupWorker
  ( runCleanupExpiredActivationKeys,
  )
where

import Control.Monad.Catch
import Data.Id (RequestId (RequestId))
import Imports
import System.Logger qualified as Log
import Wire.ActivationCodeStore.Postgres qualified as ActivationCodeStore.Postgres
import Wire.BackgroundWorker.Env (AppT, Env (..))
import Wire.Effects (runBackgroundWorkerEffects)
import Wire.ExternalAccess.External (initExtEnv)

newtype ActivationKeysCleanupError = ActivationKeysCleanupError Text
  deriving stock (Show)

instance Exception ActivationKeysCleanupError

runCleanupExpiredActivationKeys :: AppT IO ()
runCleanupExpiredActivationKeys = do
  env <- ask
  extEnv <- liftIO $ initExtEnv True
  result <-
    liftIO . runBackgroundWorkerEffects env extEnv (RequestId "activation-keys-cleanup") Nothing $
      ActivationCodeStore.Postgres.deleteExpiredActivationKeys
  case result of
    Left err -> do
      Log.err env.logger $
        Log.msg (Log.val "activation keys cleanup failed")
          . Log.field "error" err
      -- Throwing makes Arbiter retry the job (maxAttempts 3).
      liftIO . throwM $ ActivationKeysCleanupError err
    Right n ->
      Log.info env.logger $
        Log.msg (Log.val "cleaned up expired activation keys")
          . Log.field "deleted" n

{-# LANGUAGE BlockArguments #-}

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

module Wire.BackgroundWorker.ScheduledJobs (startWorker) where

import Data.Proxy (Proxy)
import Imports
import qualified Arbiter.Core as ArbiterCore
import qualified Arbiter.Hasql as ArbiterHasql
import qualified Arbiter.Migrations as ArbiterMigrations
import qualified Arbiter.Worker as ArbiterWorker
import Wire.BackgroundWorker.Env (AppT)
import Wire.BackgroundWorker.Util (CleanupAction)

startWorker :: AppT IO CleanupAction
startWorker = do
  -- Temporary anchors to keep the Arbiter packages exercised in the build
  -- until the real scheduled-job flow is wired in.
  let _coreInsertJob
        :: forall m registry payload.
           ArbiterCore.QueueOperation m registry payload =>
           ArbiterCore.JobWrite payload -> m (Maybe (ArbiterCore.JobRead payload))
      _coreInsertJob = ArbiterCore.insertJob

      _createHasqlEnv
        :: forall registry m.
           MonadIO m =>
           Proxy registry -> ByteString -> ArbiterCore.SchemaName -> m (ArbiterHasql.HasqlEnv registry)
      _createHasqlEnv = ArbiterHasql.createHasqlEnv

      _runMigrationsForRegistry
        :: forall registry.
           ArbiterCore.RegistryTables registry =>
           Proxy registry -> ByteString -> ArbiterCore.SchemaName -> ArbiterMigrations.MigrationConfig -> IO (ArbiterMigrations.MigrationResult String)
      _runMigrationsForRegistry = ArbiterMigrations.runMigrationsForRegistry

      _runWorkerPool
        :: forall m registry payload result.
           ( ArbiterWorker.JobResult result
           , MonadUnliftIO m
           , ArbiterCore.QueueOperation m registry payload
           , ArbiterCore.RegistryTables registry
           ) =>
           ArbiterWorker.WorkerConfig m payload result -> m ()
      _runWorkerPool = ArbiterWorker.runWorkerPool
  pure $ pure ()

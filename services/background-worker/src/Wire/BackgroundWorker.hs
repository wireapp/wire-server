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

module Wire.BackgroundWorker where

import Data.Metrics.Servant qualified as Metrics
import Data.Text qualified as T
import Imports
import Network.AMQP.Extended (demoteOpts)
import Network.Wai.Utilities.Server
import Servant
import Servant.Server.Generic
import UnliftIO (Concurrently (..), runConcurrently)
import Util.Options
import Wire.BackendNotificationPusher qualified as BackendNotificationPusher
import Wire.BackgroundWorker.Env
import Wire.BackgroundWorker.Health qualified as Health
import Wire.BackgroundWorker.Jobs.Consumer qualified as Jobs
import Wire.BackgroundWorker.Options
import Wire.DeadUserNotificationWatcher qualified as DeadUserNotificationWatcher
import Wire.MigrateConversations qualified as MigrateConversations

run :: Opts -> IO ()
run opts = do
  env <- mkEnv opts
  let amqpEP = either id demoteOpts opts.rabbitmq.unRabbitMqOpts
  cleanupBackendNotifPusher <-
    runAppT env $
      withNamedLogger "backend-notification-pusher" $
        BackendNotificationPusher.startWorker amqpEP
  cleanupDeadUserNotifWatcher <-
    runAppT env $
      withNamedLogger "dead-user-notification-watcher" $
        DeadUserNotificationWatcher.startWorker amqpEP
  cleanupConvMigration <-
    if opts.migrateConversations
      then
        runAppT env $
          withNamedLogger "migrate-conversations" $
            MigrateConversations.startWorker opts.migrateConversationsOptions
      else pure $ pure ()
  cleanupJobs <-
    runAppT env $
      withNamedLogger "background-job-consumer" $
        Jobs.startWorker amqpEP
  let cleanup =
        void . runConcurrently $
          (,,,)
            <$> Concurrently cleanupDeadUserNotifWatcher
            <*> Concurrently cleanupBackendNotifPusher
            <*> Concurrently cleanupConvMigration
            <*> Concurrently cleanupJobs

  let server = defaultServer (T.unpack opts.backgroundWorker.host) opts.backgroundWorker.port env.logger
  let settings = newSettings server
  -- Additional cleanup when shutting down via signals.
  runSettingsWithCleanup cleanup settings (servantApp env) Nothing

servantApp :: Env -> Application
servantApp env =
  Metrics.servantPrometheusMiddleware (Proxy @(ToServant Health.HealthAPI AsApi)) $
    genericServe $
      Health.api env

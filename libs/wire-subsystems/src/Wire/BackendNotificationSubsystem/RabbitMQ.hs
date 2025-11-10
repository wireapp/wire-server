-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.BackendNotificationSubsystem.RabbitMQ
  ( runBackendNotificationSubsystemRabbitMQ,
    BackendNotificationConfig (..),
    NoRabbitMqChannel (..),
  )
where

import Control.Monad.Catch (throwM)
import Control.Retry
import Control.Timeout
import Data.Domain
import Data.Id (RequestId)
import Data.Qualified (tDomain)
import Data.Time.Units
import Imports
import Network.AMQP qualified as Q
import Polysemy
import Wire.API.Federation.BackendNotifications (FedQueueClient, enqueue)
import Wire.BackendNotificationSubsystem

data BackendNotificationConfig = BackendNotificationConfig
  { rabbitmqChannelVar :: MVar Q.Channel,
    retryPolicy :: RetryPolicyM IO,
    channelTimeout :: Second,
    requestId :: RequestId
  }

runBackendNotificationSubsystemRabbitMQ ::
  (Member (Embed IO) r) =>
  BackendNotificationConfig ->
  InterpreterFor BackendNotificationSubsystem r
runBackendNotificationSubsystemRabbitMQ config = interpret $ \case
  SendBackendNotification originDomain targetDomain notification -> do
    let policy = config.retryPolicy
    embed @IO $
      recovering policy [logRetries (const $ pure True) (logError config)] (const (go config (tDomain originDomain) targetDomain notification))

logError ::
  BackendNotificationConfig ->
  Bool ->
  SomeException ->
  RetryStatus ->
  IO ()
logError _config willRetry e status =
  putStrLn $
    "failed to enqueue notification in RabbitMQ: "
      <> displayException e
      <> ", willRetry: "
      <> show willRetry
      <> ", retryCount: "
      <> show status.rsIterNumber

go ::
  BackendNotificationConfig ->
  Domain ->
  Domain ->
  FedQueueClient tag () ->
  IO ()
go config ownDomain remoteDomain notification = do
  mChan <- timeout config.channelTimeout (readMVar config.rabbitmqChannelVar)
  case mChan of
    Nothing -> throwM NoRabbitMqChannel
    Just chan -> enqueue chan config.requestId ownDomain remoteDomain Q.Persistent notification

data NoRabbitMqChannel = NoRabbitMqChannel
  deriving (Show)

instance Exception NoRabbitMqChannel

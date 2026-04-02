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

-- FUTUREWORK: Remove this module all together.
module Brig.Federation.Client
  ( notifyUserDeleted,
  )
where

import Brig.App
import Control.Monad
import Control.Monad.Catch (MonadMask, throwM)
import Control.Retry
import Control.Timeout
import Data.Domain
import Data.Id
import Data.Qualified
import Data.Range (Range)
import Data.Time.Units
import Imports
import Network.AMQP qualified as Q
import System.Logger.Class qualified as Log
import Wire.API.Federation.API
import Wire.API.Federation.API.Brig as FederatedBrig
import Wire.API.Federation.BackendNotifications

notifyUserDeleted ::
  ( MonadReader Env m,
    MonadIO m,
    MonadMask m,
    Log.MonadLogger m
  ) =>
  Local UserId ->
  Remote (Range 1 1000 [UserId]) ->
  m ()
notifyUserDeleted self remotes = do
  let remoteConnections = tUnqualified remotes
  let notif = UserDeletedConnectionsNotification (tUnqualified self) remoteConnections
      remoteDomain = tDomain remotes

  asks (.rabbitmqChannel) >>= \chanVar ->
    enqueueNotification (tDomain self) remoteDomain Q.Persistent chanVar $
      fedQueueClient @'OnUserDeletedConnectionsTag notif

-- | Enqueues notifications in RabbitMQ. Retries 3 times with a delay of 1s.
enqueueNotification :: (MonadIO m, MonadMask m, Log.MonadLogger m, MonadReader Env m) => Domain -> Domain -> Q.DeliveryMode -> MVar Q.Channel -> FedQueueClient c () -> m ()
enqueueNotification ownDomain remoteDomain deliveryMode chanVar action = do
  let policy = limitRetries 3 <> constantDelay 1_000_000
  recovering policy [logRetries (const $ pure True) logError] (const go)
  where
    logError willRetry (SomeException e) status = do
      rid <- asks (.requestId)
      Log.err $
        Log.msg @Text "failed to enqueue notification in RabbitMQ"
          . Log.field "error" (displayException e)
          . Log.field "willRetry" willRetry
          . Log.field "retryCount" status.rsIterNumber
          . Log.field "request" rid
    go = do
      rid <- asks (.requestId)
      mChan <- timeout (1 :: Second) (readMVar chanVar)
      case mChan of
        Nothing -> throwM NoRabbitMqChannel
        Just chan -> liftIO $ enqueue chan rid ownDomain remoteDomain deliveryMode action

data NoRabbitMqChannel = NoRabbitMqChannel
  deriving (Show)

instance Exception NoRabbitMqChannel

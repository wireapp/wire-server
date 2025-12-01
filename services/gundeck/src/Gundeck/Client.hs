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

module Gundeck.Client where

import Cassandra.Options
import Control.Lens (view)
import Data.Id
import Data.Text qualified as T
import Gundeck.Env
import Gundeck.Monad
import Gundeck.Notification.Data qualified as Notifications
import Gundeck.Push.Data qualified as Push
import Gundeck.Push.Native
import Imports
import Pulsar.Client qualified as Pulsar
import Pulsar.Client.Logging
import Pulsar.Subscription qualified as Pulsar
import System.Logger qualified as Log
import Wire.API.Notification

unregister :: UserId -> ClientId -> Gundeck ()
unregister uid cid = do
  toks <- filter byClient <$> Push.lookup uid Push.LocalQuorum
  deleteTokens toks Nothing
  where
    byClient = (cid ==) . view addrClient

removeUser :: UserId -> Gundeck ()
removeUser user = do
  toks <- Push.lookup user Push.LocalQuorum
  deleteTokens toks Nothing
  Push.erase user
  Notifications.deleteAll user

setupConsumableNotifications ::
  UserId ->
  ClientId ->
  Gundeck ()
setupConsumableNotifications uid cid = do
  let subscription = "cannon-websocket-" ++ T.unpack (clientNotificationQueueName uid cid)
      subscriptionType = Pulsar.Earliest
      topic = Pulsar.Topic . Pulsar.TopicName $ "persistent://wire/user-notifications/" ++ T.unpack (userRoutingKey uid)
  pulsarEndpoint :: Endpoint <- view pulsar
  logger <- view applog
  Pulsar.withClient (Pulsar.defaultClientConfiguration {Pulsar.clientLogger = Just (pulsarClientLogger "setupConsumableNotifications" logger)}) (toPulsarUrl pulsarEndpoint) $ do
    Pulsar.createSubscription
      ( Pulsar.defaultConsumerConfiguration
          { Pulsar.consumerType = Just Pulsar.ConsumerExclusive,
            Pulsar.consumerSubscriptionInitialPosition = Just subscriptionType
          }
      )
      subscription
      topic
      (onPulsarError "setupConsumableNotifications consumer" logger)
  Log.debug logger $
    Log.msg @String "Subscription created"
      . Log.field "topic" (show topic)
      . Log.field "subscription" (show subscription)

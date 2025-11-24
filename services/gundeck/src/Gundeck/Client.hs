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

import Control.Lens (view)
import Data.Id
import Data.Text qualified as T
import Debug.Trace
import Gundeck.Monad
import Gundeck.Notification.Data qualified as Notifications
import Gundeck.Push.Data qualified as Push
import Gundeck.Push.Native
import Imports
import Pulsar.Client qualified as Pulsar
import Pulsar.Subscription qualified as Pulsar
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
  IO ()
setupConsumableNotifications uid cid = do
  -- A hacky way to create a Pulsar subscription
  let subscription = "cannon-websocket-" ++ T.unpack (clientNotificationQueueName uid cid)
      subscriptionType = Pulsar.Earliest
      topic = Pulsar.Topic . Pulsar.TopicName $ "persistent://wire/user-notifications/" ++ T.unpack (userRoutingKey uid)
  Pulsar.withClient (Pulsar.defaultClientConfiguration {Pulsar.clientLogger = Just (pulsarClientLogger "setupConsumableNotifications")}) "pulsar://localhost:6650" $ do
    Pulsar.createSubscription
      ( Pulsar.defaultConsumerConfiguration
          { Pulsar.consumerType = Just Pulsar.ConsumerExclusive,
            Pulsar.consumerSubscriptionInitialPosition = Just subscriptionType
          }
      )
      subscription
      topic
      (onPulsarError "setupConsumableNotifications consumer")
  traceM $ "XXX - setupConsumableNotifications created subscription " <> show subscription <> " on topic " <> show topic

-- TODO: Replace Debug.Trace with regular logging
onPulsarError :: String -> Pulsar.RawResult -> IO ()
onPulsarError provenance result =
  traceM $
    provenance ++ case Pulsar.renderResult result of
      Just r -> " error: " ++ (show r)
      Nothing -> " error: " ++ (show (Pulsar.unRawResult result))

-- TODO: Replace Debug.Trace with regular logging
pulsarClientLogger :: String -> Pulsar.LogLevel -> Pulsar.LogFile -> Pulsar.LogLine -> Pulsar.LogMessage -> IO ()
pulsarClientLogger provenance level file line message = traceM $ provenance ++ " [" <> show level <> "] " <> file <> ":" <> show line <> ":" <> message

-- TODO: Replace Debug.Trace with regular logging
logPulsarResult :: String -> Pulsar.RawResult -> Pulsar.RawResult
logPulsarResult provenance result =
  trace
    ( provenance ++ case Pulsar.renderResult result of
        Just r -> " result: " ++ (show r)
        Nothing -> " result: " ++ (show (Pulsar.unRawResult result))
    )
    result

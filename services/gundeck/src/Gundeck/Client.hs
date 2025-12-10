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
import Gundeck.Env
import Gundeck.Monad
import Gundeck.Notification.Data qualified as Notifications
import Gundeck.Push.Data qualified as Push
import Gundeck.Push.Native
import Imports
import Pulsar.Admin
import Servant.Client
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
  let -- Rebuilding `latest` here. See https://github.com/apache/pulsar/blob/master/pulsar-client/src/main/java/org/apache/pulsar/client/impl/ResetCursorData.java#L58
      resetCursorCfg =
        ResetCursorData
          { resetCursorDataBatchIndex = Just (-1),
            resetCursorDataEntryId = Just $ fromIntegral (maxBound :: Int64),
            resetCursorDataExcluded = Nothing,
            resetCursorDataLedgerId = Just $ fromIntegral (maxBound :: Int64),
            resetCursorDataPartitionIndex = Just (-1),
            resetCursorDataProperties = Nothing
          }
      cfg =
        PersistentTopicsCreateSubscriptionParameters
          { persistentTopicsCreateSubscriptionTenant = "wire",
            persistentTopicsCreateSubscriptionNamespace = "user-notifications",
            persistentTopicsCreateSubscriptionTopic = userRoutingKey uid,
            persistentTopicsCreateSubscriptionSubscriptionName = ("cannon-websocket-" :: Text) <> clientNotificationQueueName uid cid,
            persistentTopicsCreateSubscriptionAuthoritative = Nothing,
            persistentTopicsCreateSubscriptionReplicated = Nothing,
            persistentTopicsCreateSubscriptionMessageId = resetCursorCfg
          }
  httpManager <- view Gundeck.Monad.manager
  pulsarAdminUrlString <- toPulsarAdminUrl <$> view pulsarAdmin
  pulsarAdminUrl <- parseBaseUrl pulsarAdminUrlString
  liftIO . void $ flip runClientM (mkClientEnv httpManager pulsarAdminUrl) $ persistentTopicsCreateSubscription cfg
  logger <- view applog
  Log.debug logger $
    Log.msg @String "Subscription created"
      . Log.field "topic" (show cfg.persistentTopicsCreateSubscriptionTopic)
      . Log.field "subscription" (show cfg.persistentTopicsCreateSubscriptionSubscriptionName)

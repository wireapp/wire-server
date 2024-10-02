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
import Data.Map qualified as Map
import Data.Text.Encoding (encodeUtf8)
import Gundeck.Monad
import Gundeck.Notification.Data qualified as Notifications
import Gundeck.Push.Data qualified as Push
import Gundeck.Push.Native
import Imports
import Network.AMQP
import Network.AMQP.Types
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
  Channel ->
  UserId ->
  ClientId ->
  IO Text
setupConsumableNotifications chan uid cid = do
  -- TODO: Do this using policies: https://www.rabbitmq.com/docs/parameters#policies
  let qName = clientNotificationQueueName uid cid
      headers =
        FieldTable $
          Map.fromList
            [ ("x-dead-letter-exchange", FVString $ encodeUtf8 userNotificationDlxName),
              ("x-dead-letter-routing-key", FVString $ encodeUtf8 userNotificationDlqName)
            ]
  void $ declareQueue chan newQueue {queueName = qName, queueHeaders = headers}
  for_ [userRoutingKey uid, clientRoutingKey uid cid] $ bindQueue chan qName userNotificationExchangeName
  pure qName

userRoutingKey :: UserId -> Text
userRoutingKey = idToText

clientRoutingKey :: UserId -> ClientId -> Text
clientRoutingKey uid cid = idToText uid <> "." <> clientToText cid

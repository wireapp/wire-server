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
import Gundeck.Monad
import Gundeck.Notification.Data qualified as Notifications
import Gundeck.Push.Data qualified as Push
import Gundeck.Push.Native
import Imports
import Network.AMQP
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
  let qName = clientNotificationQueueName uid cid
  void $
    declareQueue
      chan
      (queueOpts qName)
  for_ [userRoutingKey uid, clientRoutingKey uid cid] $
    bindQueue chan qName userNotificationExchangeName
  pure qName

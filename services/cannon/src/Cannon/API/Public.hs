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

module Cannon.API.Public
  ( publicAPIServer,
  )
where

import Cannon.App (wsapp)
import Cannon.NotificationFanIn
import Cannon.RabbitMqConsumerApp (rabbitMQWebSocketApp)
import Cannon.Types
import Cannon.WS
import Control.Monad.IO.Class
import Data.Id
import Data.Text
import GHC.Base
import Network.WebSockets.Connection
import Servant
import Wire.API.Routes.Named
import Wire.API.Routes.Public.Cannon

publicAPIServer :: ServerT CannonAPI Cannon
publicAPIServer =
  Named @"await-notifications" streamData
    :<|> Named @"websocket" streamData
    :<|> Named @"consume-events@v8" (\userId mClientId -> consumeEvents userId mClientId Nothing)
    :<|> Named @"consume-events" consumeEvents
    :<|> Named @"fan-in-events" fanInEvents

streamData :: UserId -> ConnId -> Maybe ClientId -> PendingConnection -> Cannon ()
streamData userId connId clientId con = do
  e <- wsenv
  liftIO $ wsapp (mkKey userId connId) clientId e con

consumeEvents :: UserId -> Maybe ClientId -> Maybe Text -> PendingConnection -> Cannon ()
consumeEvents userId mClientId mSyncMarker con = do
  e <- wsenv
  liftIO $ rabbitMQWebSocketApp userId mClientId mSyncMarker e con

fanInEvents :: UserId -> Maybe ClientId -> PendingConnection -> Cannon ()
fanInEvents uid cid pending = do
  (options, runtime, environment) <- fanInEnvironment
  liftIO $ fanInWebSocketApp options environment runtime uid cid pending

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

module Gundeck.API.Internal
  ( type InternalAPI,
    servantSitemap,
  )
where

import Cassandra qualified
import Control.Lens (view)
import Data.Id
import Gundeck.Client
import Gundeck.Client qualified as Client
import Gundeck.Monad
import Gundeck.Presence qualified as Presence
import Gundeck.Push qualified as Push
import Gundeck.Push.Data qualified as PushTok
import Gundeck.Push.Native.Types qualified as PushTok
import Imports
import Servant
import Wire.API.Push.Token qualified as PushTok
import Wire.API.Push.V2
import Wire.API.Routes.Internal.Gundeck
import Wire.API.Routes.Named

servantSitemap :: ServerT InternalAPI Gundeck
servantSitemap =
  Named @"i-status" statusH
    :<|> Named @"i-push" pushH
    :<|> ( Named @"i-presences-get-for-users" Presence.listAllH
             :<|> Named @"i-presences-get-for-user" Presence.listH
             :<|> Named @"i-presences-post" Presence.addH
             :<|> Named @"i-presences-delete" Presence.removeH
         )
    :<|> Named @"i-clients-delete" unregisterClientH
    :<|> Named @"i-user-delete" removeUserH
    :<|> Named @"i-push-tokens-get" getPushTokensH
    :<|> Named @"i-reg-consumable-notifs" registerConsumableNotificationsClient

statusH :: (Applicative m) => m NoContent
statusH = pure NoContent

pushH :: [Push] -> Gundeck NoContent
pushH ps = NoContent <$ Push.push ps

unregisterClientH :: UserId -> ClientId -> Gundeck NoContent
unregisterClientH uid cid = NoContent <$ Client.unregister uid cid

removeUserH :: UserId -> Gundeck NoContent
removeUserH uid = NoContent <$ Client.removeUser uid

getPushTokensH :: UserId -> Gundeck PushTok.PushTokenList
getPushTokensH uid = PushTok.PushTokenList <$> (view PushTok.addrPushToken <$$> PushTok.lookup uid Cassandra.All)

registerConsumableNotificationsClient :: UserId -> ClientId -> Gundeck NoContent
registerConsumableNotificationsClient uid cid = do
  chan <- getRabbitMqChan
  void . liftIO $ setupConsumableNotifications chan uid cid
  pure NoContent

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
  ( type GundeckInternalAPI,
    servantSitemap,
  )
where

import Cassandra qualified
import Control.Lens (view)
import Data.Aeson (eitherDecode)
import Data.CommaSeparatedList
import Data.Id
import Data.Metrics.Servant
import Data.Typeable
import Gundeck.Client qualified as Client
import Gundeck.Monad
import Gundeck.Presence qualified as Presence
import Gundeck.Push qualified as Push
import Gundeck.Push.Data qualified as PushTok
import Gundeck.Push.Native.Types qualified as PushTok
import Gundeck.Types.Presence as GD
import Gundeck.Types.Push.V2
import Imports
import Network.Wai (lazyRequestBody)
import Servant
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.DelayedIO
import Servant.Server.Internal.ErrorFormatter
import Wire.API.Push.Token qualified as PushTok
import Wire.API.Routes.Internal.Gundeck
import Wire.API.Routes.Public

servantSitemap :: ServerT GundeckInternalAPI Gundeck
servantSitemap =
  statusH
    :<|> pushH
    :<|> ( Presence.listAllH
             :<|> Presence.listH
             :<|> Presence.addH
             :<|> Presence.removeH
         )
    :<|> unregisterClientH
    :<|> removeUserH
    :<|> getPushTokensH

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

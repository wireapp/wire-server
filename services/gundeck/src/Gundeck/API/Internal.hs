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
  ( sitemap,
  )
where

import Cassandra qualified
import Control.Lens (view)
import Data.Id
import Gundeck.Client qualified as Client
import Gundeck.Monad
import Gundeck.Push qualified as Push
import Gundeck.Push.Data qualified as PushTok
import Gundeck.Push.Native.Types qualified as PushTok
import Imports hiding (head)
import Network.Wai
import Network.Wai.Predicate hiding (setStatus)
import Network.Wai.Routing hiding (route)
import Network.Wai.Utilities
import Wire.API.Push.Token qualified as PushTok

sitemap :: Routes a Gundeck ()
sitemap = do
  head "/i/status" (continue $ const (pure empty)) true
  get "/i/status" (continue $ const (pure empty)) true

  -- Push API -----------------------------------------------------------

  post "/i/push/v2" (continue pushH) $
    request .&. accept "application" "json"

  -- User-Client API -------------------------------------------------------

  delete "/i/clients/:cid" (continue unregisterClientH) $
    header "Z-User" .&. param "cid"

  delete "/i/user" (continue removeUserH) $
    header "Z-User"

  get "/i/push-tokens/:uid" (continue getPushTokensH) $
    param "uid"

type JSON = Media "application" "json"

pushH :: Request ::: JSON -> Gundeck Response
pushH (req ::: _) = do
  ps <- fromJsonBody (JsonRequest req)
  empty <$ Push.push ps

unregisterClientH :: UserId ::: ClientId -> Gundeck Response
unregisterClientH (uid ::: cid) = empty <$ Client.unregister uid cid

removeUserH :: UserId -> Gundeck Response
removeUserH uid = empty <$ Client.removeUser uid

getPushTokensH :: UserId -> Gundeck Response
getPushTokensH = fmap json . getPushTokens

getPushTokens :: UserId -> Gundeck PushTok.PushTokenList
getPushTokens uid = PushTok.PushTokenList <$> (view PushTok.addrPushToken <$$> PushTok.lookup uid Cassandra.All)

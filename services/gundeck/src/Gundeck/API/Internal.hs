-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

import Data.Id
import Data.Swagger.Build.Api hiding (Response, def, min)
import qualified Gundeck.Client as Client
import Gundeck.Monad
import qualified Gundeck.Presence as Presence
import qualified Gundeck.Push as Push
import Imports
import Network.Wai
import Network.Wai.Predicate hiding (setStatus)
import Network.Wai.Routing hiding (route)
import Network.Wai.Utilities

sitemap :: Routes ApiBuilder Gundeck ()
sitemap = do
  -- Push API -----------------------------------------------------------

  post "/i/push/v2" (continue pushH) $
    request .&. accept "application" "json"

  -- Presence API ----------------------------------------------------------

  get "/i/presences/:uid" (continue Presence.list) $
    param "uid" .&. accept "application" "json"

  get "/i/presences" (continue Presence.listAll) $
    param "ids" .&. accept "application" "json"

  post "/i/presences" (continue Presence.add) $
    request .&. accept "application" "json"

  delete "/i/presences/:uid/devices/:did/cannons/:cannon" (continue Presence.remove) $
    param "uid" .&. param "did" .&. param "cannon"

  -- User-Client API -------------------------------------------------------

  delete "/i/clients/:cid" (continue unregisterClientH) $
    header "Z-User" .&. param "cid"

  delete "/i/user" (continue removeUserH) $
    header "Z-User"

type JSON = Media "application" "json"

pushH :: Request ::: JSON -> Gundeck Response
pushH (req ::: _) = do
  ps <- fromJsonBody (JsonRequest req)
  empty <$ Push.push ps

unregisterClientH :: UserId ::: ClientId -> Gundeck Response
unregisterClientH (uid ::: cid) = empty <$ Client.unregister uid cid

removeUserH :: UserId -> Gundeck Response
removeUserH uid = empty <$ Client.removeUser uid

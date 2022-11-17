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

module Wire.API.Routes.Public.Proxy where

import Data.SOP
import qualified Data.Swagger as Swagger
import Imports
import Servant
import Servant.API.Extended.RawM (RawM)
import Servant.Swagger
import Wire.API.Routes.Named

type ProxyAPI =
  ProxyAPIRoute "youtube-path" ("youtube" :> "v3" :> RawM)
    :<|> ProxyAPIRoute "gmaps-static" ("googlemaps" :> "api" :> "staticmap" :> RawM)
    :<|> ProxyAPIRoute "gmaps-path" ("googlemaps" :> "maps" :> "api" :> "geocode" :> RawM)
    :<|> ProxyAPIRoute "giphy-path" ("giphy" :> "v1" :> "gifs" :> RawM)
    :<|> ProxyAPIRoute "spotify-token" ("spotify" :> "api" :> "token" :> RawM)
    :<|> Named
           "soundcloud-resolve"
           ( Summary (ProxyAPISummary "soundcloud-resolve")
               :> "proxy"
               :> "soundcloud"
               :> "resolve"
               :> QueryParam' '[Required, Strict] "url" Text
               :> Get '[JSON] Text
           )
    :<|> Named
           "soundcloud-stream"
           ( Summary (ProxyAPISummary "soundcloud-stream")
               :> "proxy"
               :> "soundcloud"
               :> "stream"
               :> QueryParam' '[Required, Strict] "url" Text
               :> Get '[JSON] Text
           )

type ProxyAPIRoute name path = Named name (Summary (ProxyAPISummary name) :> "proxy" :> path)

-- | API docs: if we want to make these longer, they won't clutter the routes above
-- that they document.
type family ProxyAPISummary name where
  ProxyAPISummary "youtube-path" =
    "proxy: `get /proxy/youtube/v3/:path`; see youtube API docs"
  ProxyAPISummary "gmaps-static" =
    "proxy: `get /proxy/googlemaps/api/staticmap`; see google maps API docs"
  ProxyAPISummary "gmaps-path" =
    "proxy: `get /proxy/googlemaps/maps/api/geocode/:path`; see google maps API docs"
  ProxyAPISummary "giphy-path" =
    "proxy: `get /proxy/giphy/v1/gifs/:path`; see giphy API docs"
  ProxyAPISummary "spotify-token" =
    "proxy: `get /proxy/spotify/api/token`; see spotify API docs"
  ProxyAPISummary "soundcloud-resolve" =
    "proxy: `get /proxy/soundcloud/resolve`; see soundcloud API docs"
  ProxyAPISummary "soundcloud-stream" =
    "proxy: `get /proxy/soundcloud/stream`; see soundcloud API docs"

swaggerDoc :: Swagger.Swagger
swaggerDoc = toSwagger (Proxy @ProxyAPI)

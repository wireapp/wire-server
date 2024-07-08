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

import Servant hiding (RawM)
import Servant.API.Extended.RawM (RawM)
import Wire.API.Routes.API
import Wire.API.Routes.Named

type ProxyAPI =
  ProxyAPIRoute "giphy-path" ("giphy" :> "v1" :> "gifs" :> RawM)
    :<|> ProxyAPIRoute "youtube-path" ("youtube" :> "v3" :> RawM)
    :<|> ProxyAPIRoute "gmaps-static" ("googlemaps" :> "api" :> "staticmap" :> RawM)
    :<|> ProxyAPIRoute "gmaps-path" ("googlemaps" :> "maps" :> "api" :> "geocode" :> RawM)

type ProxyAPIRoute name path = Named name (Summary (ProxyAPISummary name) :> "proxy" :> path)

-- | API docs: if we want to make these longer, they won't clutter the routes above
-- that they document.
--
-- youtube, google maps are only supported for old android.  there is no strong reason to end
-- support at any particular version, except the hope that old android won't need to support
-- V4, and if nobody uses it, we shouldn't serve it.  if you are a wire employee, see
-- https://wearezeta.atlassian.net/wiki/spaces/ENGINEERIN/pages/685867582/Proxy+for+3rd+party+services
-- for discussion.
type family ProxyAPISummary name where
  ProxyAPISummary "giphy-path" =
    "proxy: `get /proxy/giphy/v1/gifs/:path`; see giphy API docs"
  ProxyAPISummary "youtube-path" =
    "[DEPRECATED] proxy: `get /proxy/youtube/v3/:path`; see youtube API docs"
  ProxyAPISummary "gmaps-static" =
    "[DEPRECATED] proxy: `get /proxy/googlemaps/api/staticmap`; see google maps API docs"
  ProxyAPISummary "gmaps-path" =
    "[DEPRECATED] proxy: `get /proxy/googlemaps/maps/api/geocode/:path`; see google maps API docs"

data ProxyAPITag

-- | FUTUREWORK(fisx): (1) the verb could be added to the swagger docs in the appropriate
-- place here; it's always defined in the `Summary`, but the `RawM` doesn't allow to constrain
-- it.  (2) there should be a way to make this more type-safe: `assertMethod` in
-- "Proxy.API.Public" could take a type-level string literal argument containing the method,
-- and that argument could be funnelled there from the routing table somehow: `"spotify" :>
-- "api" :> "token" :> OnlyMethod "POST" :> RawM`, and then the `ServerT` instance for
-- `OnlyMethod` requires a proxy argument in the handler of the same type.  Or something.  (am
-- i massifly over-engineering things here?)
instance ServiceAPI ProxyAPITag v where
  type ServiceAPIRoutes ProxyAPITag = ProxyAPI

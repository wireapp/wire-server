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

module Wire.API.Routes.Bearer where

import qualified Data.ByteString as BS
import Data.Metrics.Servant
import qualified Data.Text.Encoding as T
import Imports
import Servant
import Servant.Swagger

newtype Bearer a = Bearer {unBearer :: a}

instance FromHttpApiData a => FromHttpApiData (Bearer a) where
  parseHeader h = case BS.splitAt 7 h of
    ("Bearer ", suffix) -> Bearer <$> parseHeader suffix
    _ -> Left "Invalid authorization scheme"
  parseUrlPiece = parseHeader . T.encodeUtf8

type BearerHeader a = Header "Authorization" (Bearer a)

instance HasSwagger api => HasSwagger (Bearer a :> api) where
  -- TODO
  toSwagger _ = toSwagger (Proxy @api)

instance RoutesToPaths api => RoutesToPaths (Bearer a :> api) where
  getRoutes = getRoutes @api

instance
  ( HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters,
    FromHttpApiData a,
    HasServer api ctx
  ) =>
  HasServer (Bearer a :> api) ctx
  where
  type ServerT (Bearer a :> api) m = Maybe a -> ServerT api m

  route _ ctx action =
    route (Proxy @(BearerHeader a :> api)) ctx (fmap (. (fmap unBearer)) action)
  hoistServerWithContext _ ctx f h = hoistServerWithContext (Proxy @api) ctx f . h

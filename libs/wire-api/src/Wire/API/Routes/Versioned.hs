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

module Wire.API.Routes.Versioned where

import Data.Aeson (FromJSON, ToJSON)
import Data.Schema 
import Data.Metrics.Servant
import qualified Data.Swagger as S
import Servant.Swagger
import Servant.Swagger.Internal
import Imports
import Servant
import Servant.API.ContentTypes
import Wire.API.Routes.Version

--------------------------------------

data VersionedReqBody' (v :: Version) (mods :: [*]) (ct :: [*]) (a :: *)

type VersionedReqBody v = VersionedReqBody' v '[Required, Strict]

instance RoutesToPaths rest => RoutesToPaths (VersionedReqBody' v mods ct a :> rest) where
  getRoutes = getRoutes @rest

instance
  ( AllCTUnrender cts (Versioned v a),
    HasServer api context,
    HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters
  ) =>
  HasServer (VersionedReqBody' v mods cts a :> api) context
  where
  type ServerT (VersionedReqBody' v mods cts a :> api) m = a -> ServerT api m

  hoistServerWithContext _p pc nt s = hoistServerWithContext p pc nt (s . unVersioned) . Versioned
        where 
         p = (Proxy :: Proxy (ReqBody cts (Versioned v a) :> api))

  route _p ctx d = route (Proxy :: Proxy (ReqBody cts (Versioned v a) :> api)) ctx (fmap (. unVersioned) d)

instance
  (HasSwagger (ReqBody' '[Required, Strict] cts a :> api),
  S.ToSchema (Versioned v a),
  HasSwagger api, AllAccept cts) =>
  HasSwagger (VersionedReqBody v cts a :> api)
  where
  toSwagger _ = toSwagger (Proxy @(ReqBody cts (Versioned v a) :> api))

newtype Versioned v a = Versioned {unVersioned :: a}

instance Functor (Versioned v) where
  fmap f (Versioned a) = Versioned (f a)

deriving via Schema (Versioned v a) instance ToSchema (Versioned v a) => FromJSON (Versioned v a)

deriving via Schema (Versioned v a) instance ToSchema (Versioned v a) => ToJSON (Versioned v a)

deriving via Schema (Versioned v a) instance ToSchema (Versioned v a) => S.ToSchema (Versioned v a)

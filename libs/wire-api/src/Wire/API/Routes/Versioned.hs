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
import Data.Kind
import Data.Metrics.Servant
import Data.Schema
import Data.Singletons
import qualified Data.Swagger as S
import GHC.TypeLits
import Imports
import Servant
import Servant.API.ContentTypes
import Servant.Swagger
import Servant.Swagger.Internal
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Version

--------------------------------------
-- Versioned requests

data VersionedReqBody' v (mods :: [Type]) (ct :: [Type]) (a :: Type)

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
      p = Proxy :: Proxy (ReqBody cts (Versioned v a) :> api)

  route _p ctx d = route (Proxy :: Proxy (ReqBody cts (Versioned v a) :> api)) ctx (fmap (. unVersioned) d)

instance
  ( HasSwagger (ReqBody' '[Required, Strict] cts a :> api),
    S.ToSchema (Versioned v a),
    HasSwagger api,
    AllAccept cts
  ) =>
  HasSwagger (VersionedReqBody v cts a :> api)
  where
  toSwagger _ = toSwagger (Proxy @(ReqBody cts (Versioned v a) :> api))

--------------------------------------------------------------------------------
-- Versioned responses

data VersionedRespond v (s :: Nat) (desc :: Symbol) (a :: Type)

type instance ResponseType (VersionedRespond v s desc a) = a

instance
  IsResponse cs (Respond s desc (Versioned v a)) =>
  IsResponse cs (VersionedRespond v s desc a)
  where
  type ResponseStatus (VersionedRespond v s desc a) = ResponseStatus (Respond s desc a)
  type ResponseBody (VersionedRespond v s desc a) = ResponseBody (Respond s desc a)

  responseRender a = responseRender @cs @(Respond s desc (Versioned v a)) a . Versioned

  responseUnrender c = fmap unVersioned . responseUnrender @cs @(Respond s desc (Versioned v a)) c

instance
  (KnownSymbol desc, S.ToSchema a) =>
  IsSwaggerResponse (VersionedRespond v s desc a)
  where
  responseSwagger = simpleResponseSwagger @a @desc

-------------------------------------------------------------------------------
-- Versioned newtype wrapper

-- Use this type to provide several JSON/swagger instances for a given type.
-- Use VersionedReqBody and VersionedRespond to select the instance to use in
-- Servant.
newtype Versioned (v :: Version) a = Versioned {unVersioned :: a}
  deriving (Eq, Show)

instance Functor (Versioned v) where
  fmap f (Versioned a) = Versioned (f a)

deriving via Schema (Versioned v a) instance ToSchema (Versioned v a) => FromJSON (Versioned v a)

deriving via Schema (Versioned v a) instance ToSchema (Versioned v a) => ToJSON (Versioned v a)

-- add version suffix to swagger schema to prevent collisions
instance (SingI v, ToSchema (Versioned v a)) => S.ToSchema (Versioned v a) where
  declareNamedSchema _ = do
    S.NamedSchema n s <- schemaToSwagger (Proxy @(Versioned v a))
    pure $ S.NamedSchema (fmap (<> toUrlPiece (demote @v)) n) s

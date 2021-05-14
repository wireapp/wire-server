{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Public where

import Control.Lens ((<>~), (?~))
import Data.Aeson hiding (json)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.Id as Id
import Data.Swagger
import GHC.Base (Symbol)
import GHC.TypeLits (KnownNat, KnownSymbol, natVal)
import Imports hiding (head)
import Servant hiding (Handler, JSON, addHeader, respond)
import Servant.Server.Internal (noContentRouter)
import Servant.Swagger (HasSwagger (toSwagger))
import Servant.Swagger.Internal (SwaggerMethod)
import Servant.Swagger.Internal.Orphans ()

-- This type exists for the special 'HasSwagger' and 'HasServer' instances. It
-- shows the "Authorization" header in the swagger docs, but expects the
-- "Z-Auth" header in the server. This helps keep the swagger docs usable
-- through nginz.
data ZUserType = ZAuthUser | ZAuthConn

type family ZUserHeader (ztype :: ZUserType) :: Symbol where
  ZUserHeader 'ZAuthUser = "Z-User"
  ZUserHeader 'ZAuthConn = "Z-Connection"

type family ZUserParam (ztype :: ZUserType) :: * where
  ZUserParam 'ZAuthUser = UserId
  ZUserParam 'ZAuthConn = ConnId

data ZAuthServant (ztype :: ZUserType)

type InternalAuth ztype =
  Header'
    '[Servant.Required, Servant.Strict]
    (ZUserHeader ztype)
    (ZUserParam ztype)

type ZUser = ZAuthServant 'ZAuthUser
type ZConn = ZAuthServant 'ZAuthConn

instance HasSwagger api => HasSwagger (ZAuthServant 'ZAuthUser :> api) where
  toSwagger _ =
    toSwagger (Proxy @api)
      & securityDefinitions <>~ InsOrdHashMap.singleton "ZAuth" secScheme
      & security <>~ [SecurityRequirement $ InsOrdHashMap.singleton "ZAuth" []]
    where
      secScheme =
        SecurityScheme
          { _securitySchemeType = SecuritySchemeApiKey (ApiKeyParams "Authorization" ApiKeyHeader),
            _securitySchemeDescription = Just "Must be a token retrieved by calling 'POST /login' or 'POST /access'. It must be presented in this format: 'Bearer \\<token\\>'."
          }

instance HasSwagger api => HasSwagger (ZAuthServant 'ZAuthConn :> api) where
  toSwagger _ = toSwagger (Proxy @api)

instance
  ( HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters,
    HasServer api ctx,
    KnownSymbol (ZUserHeader ztype),
    FromHttpApiData (ZUserParam ztype)
  ) =>
  HasServer (ZAuthServant ztype :> api) ctx
  where
  type ServerT (ZAuthServant ztype :> api) m = ServerT (InternalAuth ztype :> api) m

  route _ = Servant.route (Proxy @(InternalAuth ztype :> api))
  hoistServerWithContext _ pc nt s =
    Servant.hoistServerWithContext (Proxy @(InternalAuth ztype :> api)) pc nt s

-- FUTUREWORK: Make a PR to the servant-swagger package with this instance
instance ToSchema a => ToSchema (Headers ls a) where
  declareNamedSchema _ = declareNamedSchema (Proxy @a)

-- TODO: remove
data Empty200 = Empty200
  deriving (Generic)
  deriving (HasStatus) via (WithStatus 200 Empty200)

instance ToSchema Empty200 where
  declareNamedSchema _ = declareNamedSchema (Proxy @Text)

instance ToJSON Empty200 where
  toJSON _ = toJSON ("" :: Text)

data Empty404 = Empty404
  deriving (Generic)
  deriving (HasStatus) via (WithStatus 404 Empty404)

instance ToJSON Empty404 where
  toJSON _ = toJSON ("" :: Text)

instance ToSchema Empty404 where
  declareNamedSchema _ =
    declareNamedSchema (Proxy @Text) <&> (schema . description ?~ "user not found")

--- | Return type of an endpoint with an empty response.
---
--- In principle we could use 'WithStatus n NoContent' instead, but
--- Servant does not support it, so we would need orphan instances.
---
--- FUTUREWORK: merge with Empty200 in Brig.
data EmptyResult n = EmptyResult

instance
  (SwaggerMethod method, KnownNat n) =>
  HasSwagger (Verb method n '[] (EmptyResult n))
  where
  toSwagger _ = toSwagger (Proxy @(Verb method n '[] NoContent))

instance
  (ReflectMethod method, KnownNat n) =>
  HasServer (Verb method n '[] (EmptyResult n)) context
  where
  type ServerT (Verb method n '[] (EmptyResult n)) m = m (EmptyResult n)
  hoistServerWithContext _ _ nt s = nt s

  route Proxy _ = noContentRouter method status
    where
      method = reflectMethod (Proxy :: Proxy method)
      status = toEnum . fromInteger $ natVal (Proxy @n)

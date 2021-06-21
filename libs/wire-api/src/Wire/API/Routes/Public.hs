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

module Wire.API.Routes.Public where

import Control.Lens ((<>~))
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.Id as Id
import Data.SOP.Constraint (All)
import Data.Swagger
import GHC.Base (Symbol)
import GHC.TypeLits (KnownNat, KnownSymbol, natVal)
import Imports hiding (All, head)
import Network.HTTP.Types (Status)
import Network.Wai (responseLBS)
import Servant hiding (Handler, JSON, addHeader, respond)
import Servant.API.Modifiers (FoldLenient, FoldRequired)
import Servant.API.Status (KnownStatus, statusVal)
import Servant.API.UVerb.Union (foldMapUnion)
import Servant.Server.Internal
  ( Delayed,
    Handler,
    RouteResult (..),
    Router,
    addMethodCheck,
    leafRouter,
    methodCheck,
    noContentRouter,
    runAction,
  )
import Servant.Swagger (HasSwagger (toSwagger))
import Servant.Swagger.Internal (SwaggerMethod)
import Servant.Swagger.Internal.Orphans ()
import Wire.API.ServantSwagger

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

data ZAuthServant (ztype :: ZUserType) (opts :: [*])

type InternalAuthDefOpts = '[Servant.Required, Servant.Strict]

type InternalAuth ztype opts =
  Header'
    opts
    (ZUserHeader ztype)
    (ZUserParam ztype)

type ZUser = ZAuthServant 'ZAuthUser InternalAuthDefOpts

type ZConn = ZAuthServant 'ZAuthConn InternalAuthDefOpts

type ZOptUser = ZAuthServant 'ZAuthUser '[Servant.Strict]

instance HasSwagger api => HasSwagger (ZAuthServant 'ZAuthUser _opts :> api) where
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

instance HasSwagger api => HasSwagger (ZAuthServant 'ZAuthConn _opts :> api) where
  toSwagger _ = toSwagger (Proxy @api)

instance
  ( HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters,
    SBoolI (FoldLenient opts),
    SBoolI (FoldRequired opts),
    HasServer api ctx,
    KnownSymbol (ZUserHeader ztype),
    FromHttpApiData (ZUserParam ztype)
  ) =>
  HasServer (ZAuthServant ztype opts :> api) ctx
  where
  type ServerT (ZAuthServant ztype opts :> api) m = ServerT (InternalAuth ztype opts :> api) m

  route _ = Servant.route (Proxy @(InternalAuth ztype opts :> api))
  hoistServerWithContext _ pc nt s =
    Servant.hoistServerWithContext (Proxy @(InternalAuth ztype opts :> api)) pc nt s

-- FUTUREWORK: Make a PR to the servant-swagger package with this instance
instance ToSchema a => ToSchema (Headers ls a) where
  declareNamedSchema _ = declareNamedSchema (Proxy @a)

--- | Return type of an endpoint with an empty response.
---
--- In principle we could use 'WithStatus n NoContent' instead, but
--- Servant does not support it, so we would need orphan instances.
data EmptyResult n = EmptyResult

instance
  (SwaggerMethod method, KnownNat n) =>
  HasSwagger (Verb method n '[] (EmptyResult n))
  where
  toSwagger _ = toSwagger (Proxy @(Verb method n '[] NoContent))

instance
  ( SwaggerMethod method,
    KnownNat n,
    HasSwagger (UVerb method '[] as)
  ) =>
  HasSwagger (UVerb method '[] (EmptyResult n ': as))
  where
  toSwagger _ =
    combineSwagger
      (toSwagger (Proxy @(Verb method n '[] NoContent)))
      (toSwagger (Proxy @(UVerb method '[] as)))

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

class HasStatus a => IsEmptyResponse a

instance KnownStatus n => HasStatus (EmptyResult n) where
  type StatusOf (EmptyResult n) = n

instance KnownStatus n => IsEmptyResponse (EmptyResult n)

-- FUTUREWORK: submit this to Servant
instance
  {-# OVERLAPPING #-}
  ( ReflectMethod method,
    All IsEmptyResponse as,
    Unique (Statuses as)
  ) =>
  HasServer (UVerb method '[] as) context
  where
  type ServerT (UVerb method '[] as) m = m (Union as)
  hoistServerWithContext _ _ nt s = nt s

  route ::
    forall env.
    Proxy (UVerb method '[] as) ->
    Context context ->
    Delayed env (Server (UVerb method '[] as)) ->
    Router env
  route _proxy _ctx action = leafRouter route'
    where
      pickStatus :: All IsEmptyResponse as => Union as -> Status
      pickStatus = foldMapUnion (Proxy @IsEmptyResponse) getStatus

      getStatus :: forall a. IsEmptyResponse a => a -> Status
      getStatus _ = statusVal (Proxy @(StatusOf a))

      method = reflectMethod (Proxy @method)
      route' env request cont = do
        let action' :: Delayed env (Handler (Union as))
            action' = addMethodCheck action (methodCheck method request)
        runAction action' env request cont $ \(output :: Union as) -> do
          Route $ responseLBS (pickStatus output) mempty mempty

-- | A type-level tag that lets us omit any branch from Swagger docs.
--
-- Those are likely to be:
--
--   * Endpoints for which we can't generate Swagger docs.
--   * The endpoint that serves Swagger docs.
--   * Internal endpoints.
data OmitDocs

instance HasSwagger (OmitDocs :> a) where
  toSwagger _ = mempty

instance HasServer api ctx => HasServer (OmitDocs :> api) ctx where
  type ServerT (OmitDocs :> api) m = ServerT api m

  route _ = route (Proxy :: Proxy api)
  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt s

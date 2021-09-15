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
import Data.Swagger
import GHC.Base (Symbol)
import GHC.TypeLits (KnownSymbol)
import Imports hiding (All, head)
import Servant hiding (Handler, JSON, addHeader, respond)
import Servant.API.Modifiers (FoldLenient, FoldRequired)
import Servant.Swagger (HasSwagger (toSwagger))

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

type ZOptConn = ZAuthServant 'ZAuthConn '[Servant.Optional, Servant.Strict]

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

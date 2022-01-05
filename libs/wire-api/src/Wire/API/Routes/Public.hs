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

module Wire.API.Routes.Public
  ( -- * nginz combinators
    ZUser,
    ZLocalUser,
    ZConn,
    ZOptUser,
    ZOptConn,
    ZBot,
    ZProvider,

    -- * Swagger combinators
    OmitDocs,
  )
where

import Control.Lens (at, (.~), (<>~), (?~))
import Data.Domain
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.Id as Id
import Data.Metrics.Servant
import Data.Qualified
import Data.Swagger
import GHC.Base (Symbol)
import GHC.TypeLits (KnownSymbol)
import Imports hiding (All, head)
import qualified Network.Wai as Wai
import Servant hiding (Handler, JSON, addHeader, respond)
import Servant.API.Modifiers
import Servant.API.WebSocket
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.DelayedIO
import Servant.Swagger (HasSwagger (toSwagger))

mapRequestArgument ::
  forall mods a b.
  (SBoolI (FoldRequired mods), SBoolI (FoldLenient mods)) =>
  (a -> b) ->
  RequestArgument mods a ->
  RequestArgument mods b
mapRequestArgument f x =
  case (sbool :: SBool (FoldRequired mods), sbool :: SBool (FoldLenient mods)) of
    (STrue, STrue) -> fmap f x
    (STrue, SFalse) -> f x
    (SFalse, STrue) -> (fmap . fmap) f x
    (SFalse, SFalse) -> fmap f x

-- This type exists for the special 'HasSwagger' and 'HasServer' instances. It
-- shows the "Authorization" header in the swagger docs, but expects the
-- "Z-Auth" header in the server. This helps keep the swagger docs usable
-- through nginz.
data ZType
  = -- | Get a 'UserID' from the Z-Auth header
    ZAuthUser
  | -- | Same as 'ZAuthUser', but return a 'Local UserId' using the domain in the context
    ZLocalAuthUser
  | -- | Get a 'ConnId' from the Z-Conn header
    ZAuthConn
  | ZAuthBot
  | ZAuthProvider

class
  (KnownSymbol (ZHeader ztype), FromHttpApiData (ZParam ztype)) =>
  IsZType (ztype :: ZType) ctx
  where
  type ZHeader ztype :: Symbol
  type ZParam ztype :: *
  type ZQualifiedParam ztype :: *

  qualifyZParam :: Context ctx -> ZParam ztype -> ZQualifiedParam ztype

class HasTokenType ztype where
  -- | The expected value of the "Z-Type" header.
  tokenType :: Maybe ByteString

instance {-# OVERLAPPABLE #-} HasTokenType ztype where
  tokenType = Nothing

instance HasContextEntry ctx Domain => IsZType 'ZLocalAuthUser ctx where
  type ZHeader 'ZLocalAuthUser = "Z-User"
  type ZParam 'ZLocalAuthUser = UserId
  type ZQualifiedParam 'ZLocalAuthUser = Local UserId

  qualifyZParam ctx = toLocalUnsafe (getContextEntry ctx)

instance IsZType 'ZAuthUser ctx where
  type ZHeader 'ZAuthUser = "Z-User"
  type ZParam 'ZAuthUser = UserId
  type ZQualifiedParam 'ZAuthUser = UserId

  qualifyZParam _ = id

instance IsZType 'ZAuthConn ctx where
  type ZHeader 'ZAuthConn = "Z-Connection"
  type ZParam 'ZAuthConn = ConnId
  type ZQualifiedParam 'ZAuthConn = ConnId

  qualifyZParam _ = id

instance IsZType 'ZAuthBot ctx where
  type ZHeader 'ZAuthBot = "Z-Bot"
  type ZParam 'ZAuthBot = BotId
  type ZQualifiedParam 'ZAuthBot = BotId

  qualifyZParam _ = id

instance HasTokenType 'ZAuthBot where
  tokenType = Just "bot"

instance IsZType 'ZAuthProvider ctx where
  type ZHeader 'ZAuthProvider = "Z-Provider"
  type ZParam 'ZAuthProvider = ProviderId
  type ZQualifiedParam 'ZAuthProvider = ProviderId

  qualifyZParam _ = id

instance HasTokenType 'ZAuthProvider where
  tokenType = Just "provider"

data ZAuthServant (ztype :: ZType) (opts :: [*])

type InternalAuthDefOpts = '[Servant.Required, Servant.Strict]

type InternalAuth ztype opts =
  Header'
    opts
    (ZHeader ztype)
    (ZParam ztype)

type ZLocalUser = ZAuthServant 'ZLocalAuthUser InternalAuthDefOpts

type ZUser = ZAuthServant 'ZAuthUser InternalAuthDefOpts

type ZConn = ZAuthServant 'ZAuthConn InternalAuthDefOpts

type ZBot = ZAuthServant 'ZAuthBot InternalAuthDefOpts

type ZProvider = ZAuthServant 'ZAuthProvider InternalAuthDefOpts

type ZOptUser = ZAuthServant 'ZAuthUser '[Servant.Optional, Servant.Strict]

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

instance HasSwagger api => HasSwagger (ZAuthServant 'ZLocalAuthUser opts :> api) where
  toSwagger _ = toSwagger (Proxy @(ZAuthServant 'ZAuthUser opts :> api))

instance
  {-# OVERLAPPABLE #-}
  HasSwagger api =>
  HasSwagger (ZAuthServant ztype _opts :> api)
  where
  toSwagger _ = toSwagger (Proxy @api)

instance
  ( IsZType ztype ctx,
    HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters,
    SBoolI (FoldLenient opts),
    SBoolI (FoldRequired opts),
    HasServer api ctx
  ) =>
  HasServer (ZAuthServant ztype opts :> api) ctx
  where
  type
    ServerT (ZAuthServant ztype opts :> api) m =
      RequestArgument opts (ZQualifiedParam ztype) -> ServerT api m

  route _ ctx subserver = do
    Servant.route
      (Proxy @(InternalAuth ztype opts :> api))
      ctx
      ( fmap
          (. mapRequestArgument @opts (qualifyZParam @ztype ctx))
          (addAcceptCheck subserver (withRequest (checkType (tokenType @ztype))))
      )
    where
      checkType :: Maybe ByteString -> Wai.Request -> DelayedIO ()
      checkType token req = case (token, lookup "Z-Type" (Wai.requestHeaders req)) of
        (Just t, value)
          | value /= Just t ->
            delayedFail
              ServerError
                { errHTTPCode = 403,
                  errReasonPhrase = "Access denied",
                  errBody = "",
                  errHeaders = []
                }
        _ -> pure ()

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

instance RoutesToPaths api => RoutesToPaths (ZAuthServant ztype opts :> api) where
  getRoutes = getRoutes @api

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

instance RoutesToPaths api => RoutesToPaths (OmitDocs :> api) where
  getRoutes = getRoutes @api

instance HasSwagger WebSocketPending where
  -- TODO: More swagger info regarding WebSocket protocol?
  toSwagger _ =
    (mempty :: Swagger) & paths . at "/"
      ?~ ( mempty & get
             ?~ ( mempty
                    & summary ?~ "Websocket Endpoint"
                    & description ?~ "This endpoint is accessible via the websocket protocol."
                    & externalDocs ?~ (mempty & description ?~ "RFC 6455" & url .~ URL "https://datatracker.ietf.org/doc/html/rfc6455")
                )
         )

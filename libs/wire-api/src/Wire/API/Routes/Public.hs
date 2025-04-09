{-# OPTIONS_GHC -Wno-orphans #-}

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

module Wire.API.Routes.Public
  ( -- * nginz combinators
    ZUser,
    ZClient,
    ZLocalUser,
    ZConn,
    ZOptUser,
    ZOptClient,
    ZOptConn,
    ZBot,
    ZConversation,
    ZProvider,
    ZAccess,
    DescriptionOAuthScope,
    ZHostOpt,
    ZHostValue,
    ZAuthServant,
  )
where

import Control.Lens ((%~), (<>~), (?~))
import Data.ByteString (toStrict)
import Data.ByteString.Conversion (toByteString)
import Data.Domain
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Id as Id
import Data.Kind
import Data.Metrics.Servant
import Data.OpenApi hiding (HasServer, Header, Server)
import Data.OpenApi qualified as S
import Data.Qualified
import Data.Text.Encoding
import Data.Text.Encoding.Error
import GHC.Base (Symbol)
import GHC.TypeLits (KnownSymbol)
import Imports hiding (All, head)
import Network.Wai qualified as Wai
import Servant hiding (Handler, JSON, addHeader, respond)
import Servant.API.Modifiers
import Servant.OpenApi (HasOpenApi (toOpenApi))
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.DelayedIO
import Servant.Server.Internal.Router (Router)
import Wire.API.OAuth qualified as OAuth
import Wire.API.Routes.Version

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
  | ZAuthClient
  | -- | Get a 'ConnId' from the Z-Conn header
    ZAuthConn
  | ZAuthBot
  | ZAuthConv
  | ZAuthProvider
  | -- | (Typically short-lived) access token.
    ZAuthAccess

class HasTokenType (ztype :: ZType) where
  -- | The expected value of the "Z-Type" header.
  tokenType :: Maybe ByteString
  tokenType = Nothing

class
  ( KnownSymbol (ZHeader ztype),
    FromHttpApiData (ZParam ztype),
    HasTokenType ztype
  ) =>
  IsZType (ztype :: ZType) ctx
  where
  type ZHeader ztype :: Symbol
  type ZParam ztype :: Type
  type ZQualifiedParam ztype :: Type

  qualifyZParam :: Context ctx -> ZParam ztype -> ZQualifiedParam ztype

instance HasTokenType 'ZLocalAuthUser

instance (HasContextEntry ctx Domain) => IsZType 'ZLocalAuthUser ctx where
  type ZHeader 'ZLocalAuthUser = "Z-User"
  type ZParam 'ZLocalAuthUser = UserId
  type ZQualifiedParam 'ZLocalAuthUser = Local UserId

  qualifyZParam ctx = toLocalUnsafe (getContextEntry ctx)

instance HasTokenType 'ZAuthUser

instance IsZType 'ZAuthUser ctx where
  type ZHeader 'ZAuthUser = "Z-User"
  type ZParam 'ZAuthUser = UserId
  type ZQualifiedParam 'ZAuthUser = UserId

  qualifyZParam _ = id

instance HasTokenType 'ZAuthClient

instance IsZType 'ZAuthClient ctx where
  type ZHeader 'ZAuthClient = "Z-Client"
  type ZParam 'ZAuthClient = ClientId
  type ZQualifiedParam 'ZAuthClient = ClientId

  qualifyZParam _ = id

instance HasTokenType 'ZAuthConn

instance IsZType 'ZAuthConn ctx where
  type ZHeader 'ZAuthConn = "Z-Connection"
  type ZParam 'ZAuthConn = ConnId
  type ZQualifiedParam 'ZAuthConn = ConnId

  qualifyZParam _ = id

instance HasTokenType 'ZAuthBot where
  tokenType = Just "bot"

instance IsZType 'ZAuthBot ctx where
  type ZHeader 'ZAuthBot = "Z-Bot"
  type ZParam 'ZAuthBot = BotId
  type ZQualifiedParam 'ZAuthBot = BotId

  qualifyZParam _ = id

instance HasTokenType 'ZAuthConv

instance IsZType 'ZAuthConv ctx where
  type ZHeader 'ZAuthConv = "Z-Conversation"
  type ZParam 'ZAuthConv = ConvId
  type ZQualifiedParam 'ZAuthConv = ConvId

  qualifyZParam _ = id

instance HasTokenType 'ZAuthProvider where
  tokenType = Just "provider"

instance IsZType 'ZAuthProvider ctx where
  type ZHeader 'ZAuthProvider = "Z-Provider"
  type ZParam 'ZAuthProvider = ProviderId
  type ZQualifiedParam 'ZAuthProvider = ProviderId

  qualifyZParam _ = id

instance HasTokenType 'ZAuthAccess where
  tokenType = Just "access"

instance IsZType 'ZAuthAccess ctx where
  type ZHeader 'ZAuthAccess = "Z-User"
  type ZParam 'ZAuthAccess = UserId
  type ZQualifiedParam 'ZAuthAccess = UserId

  qualifyZParam _ = id

data ZAuthServant (ztype :: ZType) (opts :: [Type])

type InternalAuthDefOpts = '[Servant.Required, Servant.Strict]

type InternalAuth ztype opts =
  Header'
    opts
    (ZHeader ztype)
    (ZParam ztype)

type ZLocalUser = ZAuthServant 'ZLocalAuthUser InternalAuthDefOpts

type ZUser = ZAuthServant 'ZAuthUser InternalAuthDefOpts

type ZClient = ZAuthServant 'ZAuthClient InternalAuthDefOpts

type ZConn = ZAuthServant 'ZAuthConn InternalAuthDefOpts

type ZBot = ZAuthServant 'ZAuthBot InternalAuthDefOpts

type ZConversation = ZAuthServant 'ZAuthConv InternalAuthDefOpts

type ZProvider = ZAuthServant 'ZAuthProvider InternalAuthDefOpts

type ZAccess = ZAuthServant 'ZAuthAccess InternalAuthDefOpts

type ZOptUser = ZAuthServant 'ZAuthUser '[Servant.Optional, Servant.Strict]

type ZOptClient = ZAuthServant 'ZAuthClient '[Servant.Optional, Servant.Strict]

type ZOptConn = ZAuthServant 'ZAuthConn '[Servant.Optional, Servant.Strict]

-- | Optional @Z-Host@ header (added by @nginz@)
data ZHostOpt

instance (HasLink api) => HasLink (ZHostOpt :> api) where
  type MkLink (ZHostOpt :> api) a = MkLink api a
  toLink toA _ = toLink toA (Proxy :: Proxy api)

type ZHostValue = Text -- FUTUREWORK: use Data.Domain.Domain here instead of Text?

type ZOptHostHeader =
  Header' '[Servant.Optional, Strict] "Z-Host" ZHostValue

instance (HasOpenApi api) => HasOpenApi (ZHostOpt :> api) where
  toOpenApi _ = toOpenApi (Proxy @api)

type instance SpecialiseToVersion v (ZHostOpt :> api) = ZHostOpt :> SpecialiseToVersion v api

addZAuthSwagger :: OpenApi -> OpenApi
addZAuthSwagger s =
  s
    & S.components . S.securitySchemes <>~ SecurityDefinitions (InsOrdHashMap.singleton "ZAuth" secScheme)
    & security <>~ [SecurityRequirement $ InsOrdHashMap.singleton "ZAuth" []]
  where
    secScheme =
      SecurityScheme
        { _securitySchemeType = SecuritySchemeApiKey (ApiKeyParams "Authorization" ApiKeyHeader),
          _securitySchemeDescription = Just "Must be a token retrieved by calling 'POST /login' or 'POST /access'. It must be presented in this format: 'Bearer \\<token\\>'."
        }

type instance
  SpecialiseToVersion v (ZAuthServant t opts :> api) =
    ZAuthServant t opts :> SpecialiseToVersion v api

instance (HasOpenApi api) => HasOpenApi (ZAuthServant 'ZAuthUser _opts :> api) where
  toOpenApi _ = addZAuthSwagger (toOpenApi (Proxy @api))

instance (HasOpenApi api) => HasOpenApi (ZAuthServant 'ZLocalAuthUser opts :> api) where
  toOpenApi _ = addZAuthSwagger (toOpenApi (Proxy @api))

instance (HasLink endpoint) => HasLink (ZAuthServant usr opts :> endpoint) where
  type MkLink (ZAuthServant _ _ :> endpoint) a = MkLink endpoint a
  toLink toA _ = toLink toA (Proxy @endpoint)

instance
  {-# OVERLAPPABLE #-}
  (HasOpenApi api) =>
  HasOpenApi (ZAuthServant ztype _opts :> api)
  where
  toOpenApi _ = toOpenApi (Proxy @api)

instance
  ( HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters,
    HasServer api ctx
  ) =>
  HasServer (ZHostOpt :> api) ctx
  where
  type ServerT (ZHostOpt :> api) m = Maybe Text -> ServerT api m
  route ::
    Proxy (ZHostOpt :> api) ->
    Context ctx ->
    Delayed env (Server (ZHostOpt :> api)) ->
    Router env
  route _ = route (Proxy @(ZOptHostHeader :> api))
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

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
      checkType token req =
        case (token, lookup "Z-Type" (Wai.requestHeaders req)) of
          (Just t, v)
            | v /= Just t ->
                delayedFail
                  ServerError
                    { errHTTPCode = 403,
                      errReasonPhrase = "Access denied",
                      errBody = "",
                      errHeaders = []
                    }
          _ -> pure ()

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

instance (RoutesToPaths api) => RoutesToPaths (ZAuthServant ztype opts :> api) where
  getRoutes = getRoutes @api

instance (RoutesToPaths api) => RoutesToPaths (ZHostOpt :> api) where
  getRoutes = getRoutes @api

-- FUTUREWORK: Make a PR to the servant-swagger package with this instance
instance (Typeable ls, ToSchema a) => ToSchema (Headers ls a) where
  declareNamedSchema _ = declareNamedSchema (Proxy @a)

data DescriptionOAuthScope (scope :: OAuth.OAuthScope)

type instance
  SpecialiseToVersion v (DescriptionOAuthScope scope :> api) =
    DescriptionOAuthScope scope :> SpecialiseToVersion v api

instance
  (HasOpenApi api, OAuth.IsOAuthScope scope) =>
  HasOpenApi (DescriptionOAuthScope scope :> api)
  where
  toOpenApi _ = addScopeDescription @scope (toOpenApi (Proxy @api))

addScopeDescription :: forall scope. (OAuth.IsOAuthScope scope) => OpenApi -> OpenApi
addScopeDescription =
  allOperations
    . description
    %~ Just
      . ( <>
            "\nOAuth scope: `"
              <> ( decodeUtf8With lenientDecode . toStrict . toByteString $
                     OAuth.toOAuthScope @scope
                 )
              <> "`"
        )
      . fold

instance (HasServer api ctx) => HasServer (DescriptionOAuthScope scope :> api) ctx where
  type ServerT (DescriptionOAuthScope scope :> api) m = ServerT api m

  route _ = route (Proxy @api)
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

instance (RoutesToPaths api) => RoutesToPaths (DescriptionOAuthScope scope :> api) where
  getRoutes = getRoutes @api

instance HasOpenApi RawM where
  toOpenApi _ = toOpenApi (Proxy @Raw)

instance (HasOpenApi api) => HasOpenApi (CaptureAll :> api) where
  toOpenApi _ = desc $ toOpenApi (Proxy @api)
    where
      desc :: OpenApi -> OpenApi
      desc = S.allOperations . S.description ?~ "Capture all following path segments."

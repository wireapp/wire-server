{-# LANGUAGE DerivingVia #-}
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

    -- * OAuth combinators
    ZOauthUser,
    ZOAuthLocalUser,

    -- * Swagger combinators
    OmitDocs,
  )
where

import Control.Lens hiding (Context)
import Control.Monad.Except
import Crypto.JWT hiding (Context, params, uri, verify)
import Data.ByteString.Conversion (fromByteString)
import Data.Domain
import Data.Either.Combinators
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.Id as Id
import Data.Kind
import Data.Metrics.Servant
import Data.Qualified
import Data.SOP
import Data.String.Conversions (cs)
import Data.Swagger
import Data.Typeable (typeRep)
import GHC.Base (Symbol)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Imports hiding (All, exp, head)
import Network.Wai
import qualified Network.Wai as Wai
import Servant hiding (Handler, JSON, Tagged, addHeader, respond)
import Servant.API.Modifiers
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.DelayedIO
import Servant.Server.Internal.Router
import Servant.Swagger (HasSwagger (toSwagger))
import Servant.Swagger.Internal.Orphans ()
import Wire.API.OAuth
import Wire.API.Routes.Bearer

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

class
  (KnownSymbol (ZHeader ztype), FromHttpApiData (ZParam ztype)) =>
  IsZType (ztype :: ZType) ctx
  where
  type ZHeader ztype :: Symbol
  type ZParam ztype :: Type
  type ZQualifiedParam ztype :: Type

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

instance IsZType 'ZAuthClient ctx where
  type ZHeader 'ZAuthClient = "Z-Client"
  type ZParam 'ZAuthClient = ClientId
  type ZQualifiedParam 'ZAuthClient = ClientId

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

instance IsZType 'ZAuthConv ctx where
  type ZHeader 'ZAuthConv = "Z-Conversation"
  type ZParam 'ZAuthConv = ConvId
  type ZQualifiedParam 'ZAuthConv = ConvId

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

data ZAuthServant (ztype :: ZType) (opts :: [Type]) (scopes :: Maybe [OAuthScope])

type InternalAuthDefOpts = '[Servant.Required, Servant.Strict]

type InternalAuth ztype opts =
  Header'
    opts
    (ZHeader ztype)
    (ZParam ztype)

type ZLocalUser = ZAuthServant 'ZLocalAuthUser InternalAuthDefOpts 'Nothing

type ZUser = ZAuthServant 'ZAuthUser InternalAuthDefOpts 'Nothing

type ZClient = ZAuthServant 'ZAuthClient InternalAuthDefOpts 'Nothing

type ZConn = ZAuthServant 'ZAuthConn InternalAuthDefOpts 'Nothing

type ZBot = ZAuthServant 'ZAuthBot InternalAuthDefOpts 'Nothing

type ZConversation = ZAuthServant 'ZAuthConv InternalAuthDefOpts 'Nothing

type ZProvider = ZAuthServant 'ZAuthProvider InternalAuthDefOpts 'Nothing

type ZOptUser = ZAuthServant 'ZAuthUser '[Servant.Optional, Servant.Strict] 'Nothing

type ZOptClient = ZAuthServant 'ZAuthClient '[Servant.Optional, Servant.Strict] 'Nothing

type ZOptConn = ZAuthServant 'ZAuthConn '[Servant.Optional, Servant.Strict] 'Nothing

type ZOAuthLocalUser (scopes :: [OAuthScope]) = ZAuthServant 'ZLocalAuthUser InternalAuthDefOpts ('Just scopes)

type ZOauthUser (scopes :: [OAuthScope]) = ZAuthServant 'ZAuthUser InternalAuthDefOpts ('Just scopes)

instance
  (HasSwagger api, IsOAuthScopes scopes, scopes ~ (s ': ss), Typeable ztype) =>
  HasSwagger (ZAuthServant (ztype :: ZType) _opts ('Just scopes) :> api)
  where
  toSwagger _ =
    toSwagger (Proxy @(ZAuthServant ztype _opts ('Nothing :: Maybe [OAuthScope]) :> api))
      & securityDefinitions <>~ SecurityDefinitions (InsOrdHashMap.singleton "OAuth" secScheme)
      & security <>~ [SecurityRequirement $ InsOrdHashMap.singleton "OAuth" []]
      & addScopeDescription
    where
      secScheme =
        SecurityScheme
          { _securitySchemeType = SecuritySchemeApiKey (ApiKeyParams "Authorization" ApiKeyHeader),
            _securitySchemeDescription =
              Just $
                "Must be a token retrieved with an oauth handshake. It must be presented in this \
                \format: 'Bearer \\<token\\>'.\
                \Further reading: https://docs.wire.com/how-to/install/oauth.html"
          }

      addScopeDescription :: Swagger -> Swagger
      addScopeDescription = allOperations . description %~ Just . (<> "oauth scope: " <> showOAuthScopeList @scopes) . fold

instance (HasSwagger api, Typeable ztype) => HasSwagger (ZAuthServant (ztype :: ZType) _opts 'Nothing :> api) where
  toSwagger _ =
    toSwagger (Proxy @api)
      & securityDefinitions <>~ SecurityDefinitions (InsOrdHashMap.singleton "ZAuth" secScheme)
      & security <>~ [SecurityRequirement $ InsOrdHashMap.singleton "ZAuth" []]
    where
      secScheme =
        SecurityScheme
          { _securitySchemeType = SecuritySchemeApiKey (ApiKeyParams "Authorization" ApiKeyHeader),
            _securitySchemeDescription =
              Just $
                "Must be a token retrieved by calling 'POST /login' or 'POST /access'. It must be \
                \presented in this format: 'Bearer \\<token\\>'.\
                \\nExpected token type: "
                  <> (cs . show . typeRep $ (Proxy @ztype))
                  <> "\nFurther reading: https://github.com/wireapp/wire-server/blob/develop/libs/wire-api/src/Wire/API/Routes/Public.hs (search for HasSwagger instances)"
          }

instance HasLink endpoint => HasLink (ZAuthServant usr opts scopes :> endpoint) where
  type MkLink (ZAuthServant _ _ _ :> endpoint) a = MkLink endpoint a
  toLink toA _ = toLink toA (Proxy @endpoint)

-- | Handle routes that support both ZAuth and OAuth, tried in that order (scopes is Just).
instance
  ( IsZType ztype ctx,
    HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters,
    HasContextEntry ctx (Maybe JWK),
    SBoolI (FoldLenient opts),
    SBoolI (FoldRequired opts),
    HasServer api ctx,
    IsOAuthScopes (scopes :: [OAuthScope]),
    ZParam ztype ~ Id a
  ) =>
  HasServer (ZAuthServant ztype opts ('Just scopes) :> api) ctx
  where
  type
    ServerT (ZAuthServant ztype opts ('Just scopes) :> api) m =
      ZQualifiedParam ztype -> ServerT api m

  route ::
    ( IsZType ztype ctx,
      HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters,
      HasContextEntry ctx (Maybe JWK),
      SBoolI (FoldLenient opts),
      SBoolI (FoldRequired opts),
      HasServer api ctx,
      IsOAuthScopes scopes
    ) =>
    Proxy (ZAuthServant ztype opts ('Just scopes) :> api) ->
    Context ctx ->
    Delayed env (Server (ZAuthServant ztype opts ('Just scopes) :> api)) ->
    Router env
  route _ ctx subserver =
    Servant.route
      (Proxy @api)
      ctx
      (addAuthCheck subserver (withRequest (fmap (qualifyZParam @ztype ctx) . checkType' @ztype @scopes @ctx ctx (tokenType @ztype))))

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

checkType' ::
  forall ztype scopes ctx a.
  ( IsZType ztype ctx,
    HasContextEntry ctx (Maybe JWK),
    IsOAuthScopes scopes,
    ZParam ztype ~ Id a
  ) =>
  Context ctx ->
  Maybe ByteString ->
  Request ->
  DelayedIO (ZParam ztype)
checkType' ctx mTokenType req =
  case lookupHeaders of
    -- if the ztype requires a Z-Type header (instance of 'HasTokenType' returns a Just ...), we expect it to match the type we're looking for
    (Just expType, Just actType, Just t, Nothing) | expType == actType -> zauth t
    -- auth fails if the ztype doesn't match
    (Just _, _, _, _) -> delayedFailFatal error403
    -- if the ztype does not require a Z-Type header, we just care for the ZParam ('Z-User' etc.) header
    (Nothing, _, Just t, Nothing) -> zauth t
    -- if the 'Z-Oauth' header is present, we try to authenticate with OAuth
    (Nothing, Nothing, Nothing, Just t) -> oauth t
    -- any other case should fail
    (Nothing, _, _, _) -> delayedFailFatal error403
  where
    lookupHeaders :: (Maybe ByteString, Maybe ByteString, Maybe ByteString, Maybe ByteString)
    lookupHeaders = (mTokenType, lookup "Z-Type" (requestHeaders req), lookup headerName (requestHeaders req), lookup "Z-OAuth" (requestHeaders req))

    headerName :: IsString n => n
    headerName = fromString $ symbolVal (Proxy @(ZHeader ztype))

    zauth :: ByteString -> DelayedIO (ZParam ztype)
    zauth = maybe (delayedFailFatal error403) pure . fromByteString @(ZParam ztype)

    oauth :: ByteString -> DelayedIO (ZParam ztype)
    oauth = doOAuth (getContextEntry ctx) >=> either delayedFailFatal pure

    doOAuth :: Maybe JWK -> ByteString -> DelayedIO (Either ServerError (ZParam ztype))
    doOAuth mJwk h = tryOAuth
      where
        tryOAuth :: DelayedIO (Either ServerError (ZParam ztype))
        tryOAuth = do
          let jwkOrError = maybeToRight jwtError mJwk
          let tokenOrError = mapLeft invalidOAuthToken $ parseHeader h
          either (pure . Left) verifyOAuthToken $ (,) <$> tokenOrError <*> jwkOrError

        verifyOAuthToken :: (Bearer OAuthAccessToken, JWK) -> DelayedIO (Either ServerError (ZParam ztype))
        verifyOAuthToken (token, key) = do
          verifiedOrError <- mapLeft (invalidOAuthToken . cs . show) <$> liftIO (verify key (unOAuthToken . unBearer $ token))
          pure $
            verifiedOrError >>= \claimSet ->
              if hasScope @scopes claimSet
                then maybeToRight (invalidOAuthToken "Invalid token: Missing or invalid sub claim") (hcsSub claimSet)
                else Left insufficientScope

-- | Handle routes that support ZAuth, but not OAuth (scopes is Nothing).
instance
  ( IsZType ztype ctx,
    HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters,
    SBoolI (FoldLenient opts),
    SBoolI (FoldRequired opts),
    HasServer api ctx
  ) =>
  HasServer (ZAuthServant ztype opts 'Nothing :> api) ctx
  where
  type
    ServerT (ZAuthServant ztype opts 'Nothing :> api) m =
      RequestArgument opts (ZQualifiedParam ztype) -> ServerT api m

  route ::
    ( IsZType ztype ctx,
      HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters,
      SBoolI (FoldLenient opts),
      SBoolI (FoldRequired opts),
      HasServer api ctx
    ) =>
    Proxy (ZAuthServant ztype opts 'Nothing :> api) ->
    Context ctx ->
    Delayed env (Server (ZAuthServant ztype opts 'Nothing :> api)) ->
    Router env
  route _ ctx subserver = do
    Servant.route
      (Proxy @(InternalAuth ztype opts :> api))
      ctx
      ( fmap
          (. mapRequestArgument @opts (qualifyZParam @ztype ctx))
          (addAuthCheck (fmap const subserver) (withRequest (checkType (tokenType @ztype))))
      )
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

checkType :: Maybe ByteString -> Wai.Request -> DelayedIO ()
checkType token req = case (token, lookup "Z-Type" (Wai.requestHeaders req)) of
  (Just t, value) | value /= Just t -> delayedFail error403
  _ -> pure ()

error403 :: ServerError
error403 =
  ServerError
    { errHTTPCode = 403,
      errReasonPhrase = "Access denied",
      errBody = "",
      errHeaders = []
    }

instance RoutesToPaths api => RoutesToPaths (ZAuthServant ztype opts scopes :> api) where
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

--------------------------------------------------------------------------------
-- Util

insufficientScope :: ServerError
insufficientScope = err403 {errReasonPhrase = "Access denied", errBody = "Insufficient scope"}

jwtError :: ServerError
jwtError = err500 {errReasonPhrase = "jwt-error", errBody = "Internal error while handling JWT token"}

invalidOAuthToken :: Text -> ServerError
invalidOAuthToken t = err403 {errReasonPhrase = "Access denied", errBody = "Invalid token: " <> cs t}

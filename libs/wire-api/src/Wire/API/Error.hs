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
module Wire.API.Error
  ( -- * Static and dynamic error types
    DynError (..),
    dynError,
    dynErrorToWai,
    StaticError (..),
    KnownError,
    MapError,
    errorToResponse,
    errorToWai,
    errorToWaiWithMessage,
    APIError (..),

    -- * Static errors and Servant
    CanThrow,
    CanThrowMany,
    DeclaredErrorEffects,
    addErrorResponseToSwagger,
    addStaticErrorToSwagger,
    IsSwaggerError (..),
    ErrorResponse,

    -- * Static errors and Polysemy
    ErrorEffect,
    ErrorS,
    throwS,
    noteS,
    mapErrorS,
    runErrorS,
    mapToRuntimeError,
    mapToDynamicError,
  )
where

import Control.Error (hush)
import Control.Lens (at, (%~), (.~), (?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as A
import Data.HashMap.Strict.InsOrd
import Data.Kind
import Data.Metrics.Servant
import Data.OpenApi qualified as S
import Data.Proxy
import Data.SOP
import Data.Schema
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LT
import GHC.TypeLits
import Imports hiding (All)
import Network.HTTP.Types.Status qualified as HTTP
import Network.Wai.Utilities.Error qualified as Wai
import Network.Wai.Utilities.JSONResponse
import Polysemy
import Polysemy.Error
import Servant
import Servant.Client (HasClient (Client))
import Servant.Client.Core.HasClient (hoistClientMonad)
import Servant.Client.Streaming (HasClient (clientWithRoute))
import Servant.OpenApi
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named (Named)
import Wire.API.Routes.Version

-- | Runtime representation of a statically-known error.
data DynError = DynError
  { eCode :: Natural,
    eLabel :: Text,
    eMessage :: Text
  }

dynErrorToWai :: DynError -> Wai.Error
dynErrorToWai (DynError c l m) =
  Wai.mkError (toEnum (fromIntegral c)) (LT.fromStrict l) (LT.fromStrict m)

instance ToJSON DynError where
  toJSON = (.value) . toResponse

dynErrorFromWai :: Wai.Error -> DynError
dynErrorFromWai =
  DynError
    <$> fromIntegral . HTTP.statusCode . Wai.code
    <*> LT.toStrict . Wai.label
    <*> LT.toStrict . Wai.message

instance FromJSON DynError where
  parseJSON = fmap dynErrorFromWai . parseJSON

-- | A statically-known error. This is meant to be used as a kind.
data StaticError = StaticError
  { seCode :: Nat,
    seLabel :: Symbol,
    seMessage :: Symbol
  }

-- | The singleton corresponding to 'StaticError'. This is hand-written,
-- because the singletons library has problems with promoted natural numbers.
data SStaticError e where
  SStaticError ::
    (KnownNat c, KnownSymbol l, KnownSymbol msg) =>
    Proxy c ->
    Proxy l ->
    Proxy msg ->
    SStaticError ('StaticError c l msg)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema (SStaticError e)

class KnownError (e :: StaticError) where
  seSing :: SStaticError e

instance (KnownNat c, KnownSymbol l, KnownSymbol msg) => KnownError ('StaticError c l msg) where
  seSing = SStaticError Proxy Proxy Proxy

dynError' :: SStaticError e -> DynError
dynError' (SStaticError c l msg) = mkDynError c l msg

mkDynError ::
  (KnownNat c, KnownSymbol l, KnownSymbol msg) =>
  Proxy c ->
  Proxy l ->
  Proxy msg ->
  DynError
mkDynError c l msg =
  DynError
    (toEnum (fromIntegral (natVal c)))
    (Text.pack (symbolVal l))
    (Text.pack (symbolVal msg))

dynError :: forall e. (KnownError e) => DynError
dynError = dynError' $ seSing @e

staticErrorSchema :: SStaticError e -> ValueSchema NamedSwaggerDoc (SStaticError e)
staticErrorSchema e@(SStaticError c l m) =
  objectWithDocModifier "Error" addExample $
    SStaticError
      <$> (c <$ (const code .= field "code" codeSchema))
      <*> (l <$ (const label .= field "label" labelSchema))
      <*> (m <$ (const message .= field "message" schema))
  where
    err = dynError' e
    label = eLabel err
    code = eCode err
    message = eMessage err

    addExample = S.schema . S.example ?~ A.toJSON e
    labelSchema :: ValueSchema SwaggerDoc Text
    labelSchema = unnamed $ enum @Text "Label" (element label label)
    codeSchema :: ValueSchema SwaggerDoc Natural
    codeSchema = unnamed $ enum @Natural "Status" (element code code)

instance (KnownError e) => ToSchema (SStaticError e) where
  schema = staticErrorSchema seSing

data CanThrow e

data CanThrowMany (es :: [k])

instance (RoutesToPaths api) => RoutesToPaths (CanThrow err :> api) where
  getRoutes = getRoutes @api

instance (RoutesToPaths api) => RoutesToPaths (CanThrowMany errs :> api) where
  getRoutes = getRoutes @api

type instance
  SpecialiseToVersion v (CanThrow e :> api) =
    CanThrow e :> SpecialiseToVersion v api

instance (HasServer api ctx) => HasServer (CanThrow e :> api) ctx where
  type ServerT (CanThrow e :> api) m = ServerT api m

  route _ = route (Proxy @api)
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

instance (HasServer api ctx) => HasServer (CanThrowMany es :> api) ctx where
  type ServerT (CanThrowMany es :> api) m = ServerT api m

  route _ = route (Proxy @api)
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

instance
  (HasOpenApi api, IsSwaggerError e) =>
  HasOpenApi (CanThrow e :> api)
  where
  toOpenApi _ = addToOpenApi @e (toOpenApi (Proxy @api))

instance (HasClient m api) => HasClient m (CanThrow e :> api) where
  type Client m (CanThrow e :> api) = Client m api
  clientWithRoute pm _ = clientWithRoute pm $ Proxy @api
  hoistClientMonad pm _ = hoistClientMonad pm (Proxy @api)

type instance
  SpecialiseToVersion v (CanThrowMany es :> api) =
    CanThrowMany es :> SpecialiseToVersion v api

instance (HasOpenApi api) => HasOpenApi (CanThrowMany '[] :> api) where
  toOpenApi _ = toOpenApi (Proxy @api)

instance
  (HasOpenApi (CanThrowMany es :> api), IsSwaggerError e) =>
  HasOpenApi (CanThrowMany (e : es) :> api)
  where
  toOpenApi _ = addToOpenApi @e (toOpenApi (Proxy @(CanThrowMany es :> api)))

type family DeclaredErrorEffects api :: EffectRow where
  DeclaredErrorEffects (CanThrow e :> api) = (ErrorEffect e ': DeclaredErrorEffects api)
  DeclaredErrorEffects (CanThrowMany (e : es) :> api) =
    DeclaredErrorEffects (CanThrow e :> CanThrowMany es :> api)
  DeclaredErrorEffects (x :> api) = DeclaredErrorEffects api
  DeclaredErrorEffects (Named n api) = DeclaredErrorEffects api
  DeclaredErrorEffects api = '[]

errorResponseSwagger :: forall e. (Typeable e, KnownError e) => S.Response
errorResponseSwagger =
  mempty
    & S.description .~ (eMessage err <> " (label: `" <> eLabel err <> "`)")
    -- Defaulting this to JSON, as openapi3 needs something to map a schema against.
    -- This _should_ be overridden with the actual media types once we are at the
    -- point of rendering out the schemas for MultiVerb.
    -- Check the instance of `S.HasOpenApi (MultiVerb method (cs :: [Type]) as r)`
    & S.content .~ singleton mediaType mediaTypeObject
  where
    err = dynError @e
    mediaType = contentType $ Proxy @JSON
    mediaTypeObject =
      mempty
        & S.schema ?~ S.Inline (S.toSchema (Proxy @(SStaticError e)))

addErrorResponseToSwagger :: Int -> S.Response -> S.OpenApi -> S.OpenApi
addErrorResponseToSwagger code resp =
  S.allOperations
    . S.responses
    . S.responses
    . at code
    %~ Just
      . addRef
  where
    addRef :: Maybe (S.Referenced S.Response) -> S.Referenced S.Response
    addRef Nothing = S.Inline resp
    addRef (Just (S.Inline resp1)) = S.Inline (combineResponseSwagger resp1 resp)
    addRef (Just r@(S.Ref _)) = r

addStaticErrorToSwagger :: forall e. (Typeable e, KnownError e) => S.OpenApi -> S.OpenApi
addStaticErrorToSwagger =
  addErrorResponseToSwagger
    (fromIntegral (eCode (dynError @e)))
    (errorResponseSwagger @e)

type family MapError (e :: k) :: StaticError

type family ErrorEffect (e :: k) :: Effect

class IsSwaggerError e where
  addToOpenApi :: S.OpenApi -> S.OpenApi

-- | An effect for a static error type with no data.
type ErrorS e = Error (Tagged e ())

throwS :: forall e r a. (Member (ErrorS e) r) => Sem r a
throwS = throw (Tagged @e ())

noteS :: forall e r a. (Member (ErrorS e) r) => Maybe a -> Sem r a
noteS = note (Tagged @e ())

runErrorS :: forall e r a. Sem (ErrorS e : r) a -> Sem r (Maybe a)
runErrorS = fmap hush . runError @(Tagged e ())

mapErrorS ::
  forall e e' r a.
  (Member (ErrorS e') r) =>
  Sem (ErrorS e ': r) a ->
  Sem r a
mapErrorS = mapError (Tagged @e' . unTagged)

mapToRuntimeError ::
  forall e e' r a. (Member (Error e') r) => e' -> Sem (ErrorS e ': r) a -> Sem r a
mapToRuntimeError e' = mapError (const e')

mapToDynamicError ::
  forall e r a.
  (Member (Error DynError) r, KnownError (MapError e)) =>
  Sem (ErrorS e ': r) a ->
  Sem r a
mapToDynamicError = mapToRuntimeError (dynError @(MapError e))

errorToWai :: forall e. (KnownError (MapError e)) => Wai.Error
errorToWai = dynErrorToWai (dynError @(MapError e))

errorToWaiWithMessage :: forall e. (KnownError (MapError e)) => LT.Text -> Wai.Error
errorToWaiWithMessage msg = (dynErrorToWai (dynError @(MapError e))) {Wai.message = msg}

errorToResponse :: forall e. (KnownError (MapError e)) => JSONResponse
errorToResponse = toResponse (dynError @(MapError e))

class APIError e where
  toResponse :: e -> JSONResponse

instance APIError Wai.Error where
  toResponse = waiErrorToJSONResponse

instance APIError DynError where
  toResponse (DynError c l m) =
    toResponse $
      Wai.mkError (toEnum (fromIntegral c)) (LT.fromStrict l) (LT.fromStrict m)

instance APIError (SStaticError e) where
  toResponse = toResponse . dynError'

--------------------------------------------------------------------------------
-- MultiVerb support

type family RespondWithStaticError (s :: StaticError) :: Type where
  RespondWithStaticError ('StaticError s l m) = RespondAs JSON s m DynError

type family StaticErrorStatus (s :: StaticError) :: Nat where
  StaticErrorStatus ('StaticError s l m) = s

data ErrorResponse e

type instance ResponseType (ErrorResponse e) = DynError

instance
  ( ResponseBody (RespondWithStaticError (MapError e)) ~ LByteString,
    ResponseType (RespondWithStaticError (MapError e)) ~ DynError,
    IsResponse cs (RespondWithStaticError (MapError e))
  ) =>
  IsResponse cs (ErrorResponse e)
  where
  type ResponseStatus (ErrorResponse e) = StaticErrorStatus (MapError e)
  type ResponseBody (ErrorResponse e) = LByteString

  responseRender = responseRender @cs @(RespondWithStaticError (MapError e))
  responseUnrender = responseUnrender @cs @(RespondWithStaticError (MapError e))

instance (KnownError (MapError e)) => AsConstructor '[] (ErrorResponse e) where
  toConstructor _ = Nil
  fromConstructor _ = dynError @(MapError e)

instance (KnownError (MapError e), Typeable (MapError e)) => IsSwaggerResponse (ErrorResponse e) where
  responseSwagger = pure $ errorResponseSwagger @(MapError e)

instance
  (ResponseType r ~ a, KnownError (MapError e)) =>
  AsUnion '[ErrorResponse e, r] (Maybe a)
  where
  toUnion Nothing = Z (I (dynError @(MapError e)))
  toUnion (Just x) = S (Z (I x))
  fromUnion (Z (I _)) = Nothing
  fromUnion (S (Z (I x))) = Just x
  fromUnion (S (S x)) = case x of {}

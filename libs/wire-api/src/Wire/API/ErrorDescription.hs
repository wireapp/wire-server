module Wire.API.ErrorDescription where

import Control.Lens (at, over, (.~), (?~))
import Control.Lens.Combinators (_Just)
import qualified Data.Aeson as A
import Data.Schema
import Data.Swagger (Swagger)
import qualified Data.Swagger as Swagger
import qualified Data.Text as Text
import GHC.TypeLits (KnownSymbol, Symbol, natVal, symbolVal)
import GHC.TypeNats (Nat)
import Imports hiding (head)
import Servant hiding (Handler, JSON, addHeader, contentType, respond)
import Servant.API.Status (KnownStatus)
import Servant.Swagger.Internal
import Wire.API.ServantSwagger

-- This can be added to an endpoint to document a possible failure
-- case outside its return type (usually through an exception).
--
-- Note that there is no static check for these annotations. The set of
-- exceptions that a handler might throw can be completely independent from the
-- set of exceptions reported by 'CanThrow', as far as the compiler is concerned.
data CanThrow err

instance
  (HasSwagger api, KnownStatus code, KnownSymbol desc) =>
  HasSwagger (CanThrow (ErrorDescription code desc) :> api)
  where
  toSwagger _ = errorDescriptionAddToSwagger @code @desc (toSwagger (Proxy @api))

-- CanThrow annotations are ignored by servant
instance
  ( HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters,
    HasServer api ctx
  ) =>
  HasServer (CanThrow t :> api) ctx
  where
  type ServerT (CanThrow t :> api) m = ServerT api m

  route _ = route (Proxy @api)
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

errorDescriptionAddToSwagger ::
  forall (code :: Nat) (desc :: Symbol).
  (KnownStatus code, KnownSymbol desc) =>
  Swagger ->
  Swagger
errorDescriptionAddToSwagger =
  over (Swagger.paths . traverse) overridePathItem
  where
    overridePathItem :: Swagger.PathItem -> Swagger.PathItem
    overridePathItem =
      over (Swagger.get . _Just) overrideOp
        . over (Swagger.post . _Just) overrideOp
        . over (Swagger.put . _Just) overrideOp
        . over (Swagger.head_ . _Just) overrideOp
        . over (Swagger.patch . _Just) overrideOp
        . over (Swagger.delete . _Just) overrideOp
        . over (Swagger.options . _Just) overrideOp
    overrideOp :: Swagger.Operation -> Swagger.Operation
    overrideOp =
      Swagger.responses . Swagger.responses . at (fromInteger $ natVal (Proxy @code))
        ?~ Swagger.Inline
          ( mempty
              & Swagger.description .~ Text.pack (symbolVal (Proxy @desc))
              & Swagger.schema ?~ Swagger.toSchemaRef (Proxy @(ErrorDescription code desc))
          )

-- FUTUREWORK: Ponder about elevating label and messge to the type level. If all
-- errors are static, there is probably no point in having them at value level.
data ErrorDescription (statusCode :: Nat) (desc :: Symbol) = ErrorDescription
  { label :: !Text,
    message :: !Text
  }
  deriving stock (Show, Typeable)
  deriving (A.ToJSON, A.FromJSON, Swagger.ToSchema) via Schema (ErrorDescription statusCode desc)

instance (KnownStatus statusCode, KnownSymbol desc) => ToSchema (ErrorDescription statusCode desc) where
  schema =
    object "ErrorDescription" $
      ErrorDescription
        <$> label .= field "label" schema
        <*> message .= field "message" schema
        <* const (natVal (Proxy @statusCode)) .= field "code" schema

-- | This instance works with 'UVerb' only becaue of the following overlapping
-- instance for 'UVerb method cs (ErrorDescription status desc ': rest))'
instance (KnownStatus statusCode, KnownSymbol desc, AllAccept cs, SwaggerMethod method) => HasSwagger (Verb method statusCode cs (ErrorDescription statusCode desc)) where
  toSwagger _ = overrideResponseDesc $ mkEndpoint "/" (Proxy @(Verb method statusCode cs (Headers '[] (ErrorDescription statusCode desc))))
    where
      overrideResponseDesc :: Swagger -> Swagger
      overrideResponseDesc = errorDescriptionAddToSwagger @statusCode @desc

-- | This is a copy of instance for 'UVerb method cs (a:as)', but without this
-- things don't work because the instance defined in the library is already
-- compiled with the now overlapped version of `Verb method cs a` and won't
-- pickup the above instance.
instance
  (KnownStatus status, KnownSymbol desc, AllAccept cs, SwaggerMethod method, HasSwagger (UVerb method cs rest)) =>
  HasSwagger (UVerb method cs (ErrorDescription status desc ': rest))
  where
  toSwagger _ =
    toSwagger (Proxy @(Verb method (StatusOf (ErrorDescription status desc)) cs (ErrorDescription status desc)))
      `combineSwagger` toSwagger (Proxy @(UVerb method cs rest))

instance (KnownStatus status, KnownStatus status) => HasStatus (ErrorDescription status desc) where
  type StatusOf (ErrorDescription status desc) = status

-- * Errors

type ConversationNotFound = ErrorDescription 404 "Conversation not found"

convNotFound :: ConversationNotFound
convNotFound = ErrorDescription "no-conversation" "conversation not found"

type UnknownClient = ErrorDescription 403 "Unknown Client"

unknownClient :: UnknownClient
unknownClient = ErrorDescription "unknown-client" "Sending client not known"

type ClientNotFound = ErrorDescription 404 "Client not found"

clientNotFound :: ClientNotFound
clientNotFound = ErrorDescription "client-not-found" "client not found"

type NotConnected = ErrorDescription 403 "Users are not connected"

notConnected :: NotConnected
notConnected = ErrorDescription "not-connected" "Users are not connected"

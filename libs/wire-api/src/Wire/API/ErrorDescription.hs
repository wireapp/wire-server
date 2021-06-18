module Wire.API.ErrorDescription where

import Control.Lens (at, over, (.~), (?~))
import Control.Lens.Combinators (_Just)
import qualified Data.Aeson as A
import Data.Schema
import Data.Swagger (Swagger)
import qualified Data.Swagger as Swagger
import qualified Data.Text as Text
import GHC.TypeLits (KnownNat, KnownSymbol, Symbol, natVal, symbolVal)
import GHC.TypeNats (Nat)
import Imports hiding (head)
import Servant hiding (Handler, JSON, addHeader, contentType, respond)
import Servant.API.Status (KnownStatus)
import Servant.Swagger.Internal
import Wire.API.ServantSwagger

-- FUTUREWORK: Ponder about elevating label and messge to the type level. If all
-- errors are static, there is probably no point in having them at value level.
data ErrorDescription (statusCode :: Nat) (desc :: Symbol) = ErrorDescription
  { label :: !Text,
    message :: !Text
  }
  deriving stock (Show, Typeable)
  deriving (A.ToJSON, A.FromJSON, Swagger.ToSchema) via Schema (ErrorDescription statusCode desc)

instance (KnownNat statusCode, KnownSymbol desc) => ToSchema (ErrorDescription statusCode desc) where
  schema =
    object "ErrorDescription" $
      ErrorDescription
        <$> label .= field "label" schema
        <*> message .= field "message" schema
        <* const (natVal (Proxy @statusCode)) .= field "code" schema

-- | This instance works with 'UVerb' only becaue of the following overlapping
-- instance for 'UVerb method cs (ErrorDescription status desc ': rest))'
instance (KnownNat statusCode, KnownSymbol desc, AllAccept cs, SwaggerMethod method) => HasSwagger (Verb method statusCode cs (ErrorDescription statusCode desc)) where
  toSwagger _ = overrrideResponseDesc $ mkEndpoint "/" (Proxy @(Verb method statusCode cs (Headers '[] (ErrorDescription statusCode desc))))
    where
      overrrideResponseDesc :: Swagger -> Swagger
      overrrideResponseDesc =
        over (Swagger.paths . at "/" . _Just) overridePathItem
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
        Swagger.responses . Swagger.responses . at (fromInteger $ natVal (Proxy @statusCode))
          ?~ Swagger.Inline
            ( mempty
                & Swagger.description .~ Text.pack (symbolVal (Proxy @desc))
                & Swagger.schema ?~ Swagger.toSchemaRef (Proxy @(ErrorDescription statusCode desc))
            )

-- | This is a copy of instance for 'UVerb method cs (a:as)', but without this
-- things don't work because the instance defined in the library is already
-- compiled with the now overlapped version of `Verb method cs a` and won't
-- pickup the above instance.
instance
  (KnownNat status, KnownSymbol desc, AllAccept cs, SwaggerMethod method, HasSwagger (UVerb method cs rest)) =>
  HasSwagger (UVerb method cs (ErrorDescription status desc ': rest))
  where
  toSwagger _ =
    toSwagger (Proxy @(Verb method (StatusOf (ErrorDescription status desc)) cs (ErrorDescription status desc)))
      `combineSwagger` toSwagger (Proxy @(UVerb method cs rest))

instance (KnownNat status, KnownStatus status) => HasStatus (ErrorDescription status desc) where
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

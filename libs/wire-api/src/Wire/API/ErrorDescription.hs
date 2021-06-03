module Wire.API.ErrorDescription where

import Control.Lens (at, over, (.~), (?~))
import Control.Lens.Combinators (_Just)
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.Schema
import Data.Swagger (PathItem (..), Swagger (..))
import qualified Data.Swagger as Swagger
import qualified Data.Text as Text
import GHC.TypeLits (KnownNat, KnownSymbol, Symbol, natVal, symbolVal)
import GHC.TypeNats (Nat)
import Imports hiding (head)
import Servant hiding (Handler, JSON, addHeader, contentType, respond)
import Servant.API.Status (KnownStatus)
import Servant.Swagger.Internal

data ErrorDescription (status :: Nat) (desc :: Symbol) = ErrorDescription
  { label :: !Text,
    message :: !Text
  }
  deriving stock (Show, Typeable)
  deriving (A.ToJSON, A.FromJSON, Swagger.ToSchema) via Schema (ErrorDescription status desc)

instance (KnownNat status, KnownSymbol desc) => ToSchema (ErrorDescription status desc) where
  schema =
    addDoc $
      object "ErrorDescription" $
        ErrorDescription
          <$> label .= field "label" schema
          <*> message .= field "message" schema
          <* const (natVal (Proxy @status)) .= field "status" schema
    where
      -- FUTUREWORK: Make this description go into swagger's response
      -- description
      addDoc sch =
        sch
          & Swagger.schema . Swagger.description ?~ Text.pack (symbolVal (Proxy @desc))

-- | This insance works with 'UVerb' only becaue of the following overlapping
-- instance for 'UVerb method cs (ErrorDescription status desc ': rest))'
instance (KnownNat status, KnownSymbol desc, AllAccept cs, SwaggerMethod method) => HasSwagger (Verb method status cs (ErrorDescription status desc)) where
  toSwagger _ = overrrideResponseDesc $ mkEndpoint "/" (Proxy @(Verb method status cs (Headers '[] (ErrorDescription status desc))))
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
        Swagger.responses . Swagger.responses . at (fromInteger $ natVal (Proxy @status))
          ?~ Swagger.Inline
            ( mempty
                & Swagger.description .~ Text.pack (symbolVal (Proxy @desc))
                & Swagger.schema ?~ Swagger.toSchemaRef (Proxy @(ErrorDescription status desc))
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
    where
      -- workaround for https://github.com/GetShopTV/swagger2/issues/218
      -- We'd like to juse use (<>) but the instances are wrong
      combinePathItem :: PathItem -> PathItem -> PathItem
      combinePathItem s t =
        PathItem
          { _pathItemGet = _pathItemGet s <> _pathItemGet t,
            _pathItemPut = _pathItemPut s <> _pathItemPut t,
            _pathItemPost = _pathItemPost s <> _pathItemPost t,
            _pathItemDelete = _pathItemDelete s <> _pathItemDelete t,
            _pathItemOptions = _pathItemOptions s <> _pathItemOptions t,
            _pathItemHead = _pathItemHead s <> _pathItemHead t,
            _pathItemPatch = _pathItemPatch s <> _pathItemPatch t,
            _pathItemParameters = _pathItemParameters s <> _pathItemParameters t
          }

      combineSwagger :: Swagger -> Swagger -> Swagger
      combineSwagger s t =
        Swagger
          { _swaggerInfo = _swaggerInfo s <> _swaggerInfo t,
            _swaggerHost = _swaggerHost s <|> _swaggerHost t,
            _swaggerBasePath = _swaggerBasePath s <|> _swaggerBasePath t,
            _swaggerSchemes = _swaggerSchemes s <> _swaggerSchemes t,
            _swaggerConsumes = _swaggerConsumes s <> _swaggerConsumes t,
            _swaggerProduces = _swaggerProduces s <> _swaggerProduces t,
            _swaggerPaths = InsOrdHashMap.unionWith combinePathItem (_swaggerPaths s) (_swaggerPaths t),
            _swaggerDefinitions = _swaggerDefinitions s <> _swaggerDefinitions t,
            _swaggerParameters = _swaggerParameters s <> _swaggerParameters t,
            _swaggerResponses = _swaggerResponses s <> _swaggerResponses t,
            _swaggerSecurityDefinitions = _swaggerSecurityDefinitions s <> _swaggerSecurityDefinitions t,
            _swaggerSecurity = _swaggerSecurity s <> _swaggerSecurity t,
            _swaggerTags = _swaggerTags s <> _swaggerTags t,
            _swaggerExternalDocs = _swaggerExternalDocs s <|> _swaggerExternalDocs t
          }

instance (KnownNat status, KnownStatus status) => HasStatus (ErrorDescription status desc) where
  type StatusOf (ErrorDescription status desc) = status

-- * Errors

type ConversationNotFound = ErrorDescription 404 "Conversation not found"

convNotFound :: ConversationNotFound
convNotFound = ErrorDescription "no-conversation" "conversation not found"

type UnknownClient = ErrorDescription 403 "Unknown Client"

unknownClient :: UnknownClient
unknownClient = ErrorDescription "unknown-client" "Sending client not known"


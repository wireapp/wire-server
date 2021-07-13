module Wire.API.ErrorDescription where

import Control.Lens (at, ix, over, (%~), (.~), (<>~), (?~))
import Control.Lens.Combinators (_Just)
import qualified Data.Aeson as A
import Data.SOP (I (..), NS (..))
import Data.Schema
import Data.Swagger (Swagger)
import qualified Data.Swagger as Swagger
import qualified Data.Text as Text
import GHC.TypeLits (KnownSymbol, Symbol, natVal, symbolVal)
import GHC.TypeNats (Nat)
import Imports hiding (head)
import Servant hiding (Handler, addHeader, contentType, respond)
import Servant.API.Status (KnownStatus, statusVal)
import Servant.Swagger.Internal
import Wire.API.Routes.MultiVerb
import Wire.API.ServantSwagger

-- This can be added to an endpoint to document a possible failure
-- case outside its return type (usually through an exception).
--
-- Note that there is no static check for these annotations. The set of
-- exceptions that a handler might throw can be completely independent from the
-- set of exceptions reported by 'CanThrow', as far as the compiler is concerned.
data CanThrow (err :: *)

instance
  (HasSwagger api, KnownStatus code, KnownSymbol label, KnownSymbol desc) =>
  HasSwagger (CanThrow (ErrorDescription code label desc) :> api)
  where
  toSwagger _ = errorDescriptionAddToSwagger @code @label @desc (toSwagger (Proxy @api))

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
  forall (code :: Nat) (label :: Symbol) (desc :: Symbol).
  (KnownStatus code, KnownSymbol label, KnownSymbol desc) =>
  Swagger ->
  Swagger
errorDescriptionAddToSwagger =
  over (Swagger.paths . traverse) overridePathItem
  where
    addRef ::
      Maybe (Swagger.Referenced Swagger.Response) ->
      Maybe (Swagger.Referenced Swagger.Response)
    addRef Nothing =
      Just . Swagger.Inline $
        mempty
          & Swagger.description .~ Text.pack (symbolVal (Proxy @desc))
          & Swagger.schema ?~ Swagger.Inline (Swagger.toSchema (Proxy @(ErrorDescription code label desc)))
    addRef (Just response) =
      Just $
        response
          -- add the description of this error to the response description
          & Swagger._Inline . Swagger.description
            <>~ ("\n\n" <> Text.pack (symbolVal (Proxy @desc)))
          -- add the label of this error to the possible values of the corresponding enum
          & Swagger._Inline . Swagger.schema . _Just . Swagger._Inline . Swagger.properties . ix "label" . Swagger._Inline . Swagger.enum_ . _Just
            <>~ [A.toJSON (symbolVal (Proxy @label))]

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
        %~ addRef

-- FUTUREWORK: Ponder about elevating label and messge to the type level. If all
-- errors are static, there is probably no point in having them at value level.
data ErrorDescription (statusCode :: Nat) (label :: Symbol) (desc :: Symbol) = ErrorDescription {edMessage :: Text}
  deriving stock (Show, Typeable)
  deriving (A.ToJSON, A.FromJSON, Swagger.ToSchema) via Schema (ErrorDescription statusCode label desc)

instance (KnownStatus statusCode, KnownSymbol label, KnownSymbol desc) => ToSchema (ErrorDescription statusCode label desc) where
  schema =
    objectWithDocModifier "ErrorDescription" addExample $
      ErrorDescription
        <$ const label .= field "label" labelSchema
          <*> edMessage .= field "message" schema
          <* const code .= field "code" codeSchema
    where
      label = Text.pack (symbolVal (Proxy @label))
      code :: Integer
      code = natVal (Proxy @statusCode)
      desc = Text.pack (symbolVal (Proxy @desc))
      addExample =
        Swagger.schema . Swagger.example
          ?~ A.toJSON (ErrorDescription @statusCode @label @desc desc)
      labelSchema :: ValueSchema SwaggerDoc Text
      labelSchema = unnamed $ enum @Text "Label" (element label label)
      codeSchema :: ValueSchema SwaggerDoc Integer
      codeSchema = unnamed $ enum @Integer "Status" (element code code)

-- | This instance works with 'UVerb' only because of the following overlapping
-- instance for 'UVerb method cs (ErrorDescription status label desc ': rest))'
instance
  (KnownStatus statusCode, KnownSymbol label, KnownSymbol desc, AllAccept cs, SwaggerMethod method) =>
  HasSwagger (Verb method statusCode cs (ErrorDescription statusCode label desc))
  where
  toSwagger _ =
    mempty
      & Swagger.paths . at "/"
        ?~ ( mempty & method
               ?~ ( mempty
                      & Swagger.produces ?~ Swagger.MimeList responseContentTypes
                      & at code
                        ?~ Swagger.Inline
                          ( mempty
                              & Swagger.description .~ desc
                              & Swagger.schema ?~ schemaRef
                          )
                  )
           )
    where
      method = swaggerMethod (Proxy @method)
      responseContentTypes = allContentType (Proxy @cs)
      code = fromIntegral (natVal (Proxy @statusCode))
      desc = Text.pack (symbolVal (Proxy @desc))
      schemaRef = Swagger.Inline $ Swagger.toSchema (Proxy @(ErrorDescription statusCode label desc))

-- | This is a copy of instance for 'UVerb method cs (a:as)', but without this
-- things don't work because the instance defined in the library is already
-- compiled with the now overlapped version of `Verb method cs a` and won't
-- pickup the above instance.
instance
  (KnownStatus status, KnownSymbol label, KnownSymbol desc, AllAccept cs, SwaggerMethod method, HasSwagger (UVerb method cs rest)) =>
  HasSwagger (UVerb method cs (ErrorDescription status label desc ': rest))
  where
  toSwagger _ =
    toSwagger (Proxy @(Verb method status cs (ErrorDescription status label desc)))
      `combineSwagger` toSwagger (Proxy @(UVerb method cs rest))

instance KnownStatus status => HasStatus (ErrorDescription status label desc) where
  type StatusOf (ErrorDescription status label desc) = status

-- * MultiVerb errors

type RespondWithErrorDescription s label desc =
  Respond '[JSON] s desc (ErrorDescription s label desc)

instance
  ( KnownStatus s,
    KnownSymbol label,
    KnownSymbol desc
  ) =>
  IsResponse (ErrorDescription s label desc)
  where
  type ResponseContentTypes (ErrorDescription s label desc) = '[JSON]
  type ResponseType (ErrorDescription s label desc) = ErrorDescription s label desc

  responseRender = responseRender @(RespondWithErrorDescription s label desc)
  responseSwagger = responseSwagger @(RespondWithErrorDescription s label desc)

instance
  (ResponseType r ~ a, KnownSymbol desc) =>
  AsUnion
    '[ErrorDescription s label desc, r]
    (Maybe a)
  where
  asUnion Nothing = Z (I mkErrorDescription)
  asUnion (Just x) = S (Z (I x))

-- * Empty errors for legacy reasons

data EmptyErrorForLegacyReasons s desc

instance
  ( KnownStatus s,
    KnownSymbol desc
  ) =>
  IsResponse (EmptyErrorForLegacyReasons s desc)
  where
  type ResponseContentTypes (EmptyErrorForLegacyReasons s desc) = '[PlainText]
  type ResponseType (EmptyErrorForLegacyReasons s desc) = ()

  responseRender () = [RenderOutput (statusVal (Proxy @s)) mempty mempty]
  responseSwagger =
    pure $
      ResponseSwagger
        { rsDescription =
            Text.pack (symbolVal (Proxy @desc)) <> "\n\n"
              <> "**Note**: This error has an empty body for legacy reasons",
          rsStatus = statusVal (Proxy @s),
          rsHeaders = mempty,
          rsSchema = Nothing
        }

instance
  ( ResponseType r ~ a,
    KnownStatus s,
    KnownSymbol desc
  ) =>
  AsUnion
    '[EmptyErrorForLegacyReasons s desc, r]
    (Maybe a)
  where
  asUnion Nothing = Z (I ())
  asUnion (Just x) = S (Z (I x))

-- * Errors

mkErrorDescription :: forall code label desc. KnownSymbol desc => ErrorDescription code label desc
mkErrorDescription = ErrorDescription $ Text.pack (symbolVal (Proxy @desc))

type ConvNotFound = ErrorDescription 404 "no-conversation" "Conversation not found"

convNotFound :: ConvNotFound
convNotFound = mkErrorDescription

type UnknownClient = ErrorDescription 403 "unknown-client" "Unknown Client"

unknownClient :: UnknownClient
unknownClient = ErrorDescription "Sending client not known"

type ClientNotFound = ErrorDescription 404 "client-not-found" "Client not found"

clientNotFound :: ClientNotFound
clientNotFound = mkErrorDescription

type NotConnected = ErrorDescription 403 "not-connected" "Users are not connected"

notConnected :: NotConnected
notConnected = mkErrorDescription

type OperationDenied = ErrorDescription 403 "operation-denied" "Insufficient permissions"

operationDenied :: Show perm => perm -> OperationDenied
operationDenied p =
  ErrorDescription $
    "Insufficient permissions (missing " <> Text.pack (show p) <> ")"

type NotATeamMember = ErrorDescription 403 "no-team-member" "Requesting user is not a team member"

notATeamMember :: NotATeamMember
notATeamMember = mkErrorDescription

type ActionDenied = ErrorDescription 403 "action-denied" "Insufficient authorization"

actionDenied :: Show a => a -> ActionDenied
actionDenied a =
  ErrorDescription $
    "Insufficient authorization (missing " <> Text.pack (show a) <> ")"

type CodeNotFound = ErrorDescription 404 "no-conversation-code" "Conversation code not found"

codeNotFound :: CodeNotFound
codeNotFound = mkErrorDescription

type ConvAccessDenied = ErrorDescription 403 "access-denied" "Conversation access denied"

convAccessDenied :: ConvAccessDenied
convAccessDenied = mkErrorDescription

type HandleNotFound = ErrorDescription 404 "not-found" "Handle not found"

handleNotFound :: HandleNotFound
handleNotFound = mkErrorDescription

type TooManyClients = ErrorDescription 403 "too-many-clients" "Too many clients"

tooManyClients :: TooManyClients
tooManyClients = mkErrorDescription

type MissingAuth =
  ErrorDescription
    403
    "missing-auth"
    "Re-authentication via password required"

missingAuthError :: MissingAuth
missingAuthError = mkErrorDescription

type MalformedPrekeys = ErrorDescription 400 "bad-request" "Malformed prekeys uploaded"

malformedPrekeys :: MalformedPrekeys
malformedPrekeys = mkErrorDescription

module Wire.API.ErrorDescription where

import Control.Lens (at, (%~), (.~), (<>~), (?~))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Data.SOP (I (..), NP (..), NS (..))
import Data.Schema
import Data.Swagger (Swagger)
import qualified Data.Swagger as S
import qualified Data.Swagger.Declare as S
import qualified Data.Text as Text
import GHC.TypeLits (KnownSymbol, Symbol, natVal, symbolVal)
import GHC.TypeNats (Nat)
import Imports hiding (head)
import Servant hiding (Handler, addHeader, contentType, respond)
import Servant.API (contentType)
import Servant.API.ContentTypes (AllMimeRender, AllMimeUnrender)
import Servant.API.Status (KnownStatus, statusVal)
import Servant.Swagger.Internal
import Wire.API.Routes.MultiVerb

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
  (S.allOperations . S.responses . S.responses . at status %~ Just . addRef)
    . (S.definitions <>~ defs)
  where
    addRef :: Maybe (S.Referenced S.Response) -> S.Referenced S.Response
    addRef Nothing = S.Inline resp
    addRef (Just (S.Inline resp1)) = S.Inline (combineResponseSwagger resp1 resp)
    addRef (Just r@(S.Ref _)) = r

    status = fromInteger (natVal (Proxy @code))
    (defs, resp) =
      S.runDeclare (responseSwagger @(ErrorDescription code label desc)) mempty

-- FUTUREWORK: Ponder about elevating label and messge to the type level. If all
-- errors are static, there is probably no point in having them at value level.
data ErrorDescription (statusCode :: Nat) (label :: Symbol) (desc :: Symbol) = ErrorDescription {edMessage :: Text}
  deriving stock (Show, Typeable)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema (ErrorDescription statusCode label desc)

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
        S.schema . S.example
          ?~ A.toJSON (ErrorDescription @statusCode @label @desc desc)
      labelSchema :: ValueSchema SwaggerDoc Text
      labelSchema = unnamed $ enum @Text "Label" (element label label)
      codeSchema :: ValueSchema SwaggerDoc Integer
      codeSchema = unnamed $ enum @Integer "Status" (element code code)

instance KnownStatus status => HasStatus (ErrorDescription status label desc) where
  type StatusOf (ErrorDescription status label desc) = status

-- * MultiVerb errors

type RespondWithErrorDescription s label desc =
  Respond s desc (ErrorDescription s label desc)

type instance ResponseType (ErrorDescription s label desc) = ErrorDescription s label desc

instance
  ( AllMimeRender cs (ErrorDescription s label desc),
    AllMimeUnrender cs (ErrorDescription s label desc),
    KnownStatus s,
    KnownSymbol label,
    KnownSymbol desc
  ) =>
  IsResponse cs (ErrorDescription s label desc)
  where
  type ResponseStatus (ErrorDescription s label desc) = s

  responseRender = responseRender @cs @(RespondWithErrorDescription s label desc)
  responseUnrender = responseUnrender @cs @(RespondWithErrorDescription s label desc)

instance KnownSymbol desc => AsConstructor '[] (ErrorDescription s label desc) where
  toConstructor _ = Nil
  fromConstructor _ = mkErrorDescription

instance
  (KnownStatus s, KnownSymbol label, KnownSymbol desc) =>
  IsSwaggerResponse (ErrorDescription s label desc)
  where
  responseSwagger =
    pure $
      mempty
        & S.description .~ desc
        & S.schema ?~ S.Inline (S.toSchema (Proxy @(ErrorDescription s label desc)))
    where
      desc =
        Text.pack (symbolVal (Proxy @desc))
          <> " (label: `"
          <> Text.pack (symbolVal (Proxy @label))
          <> "`)"

instance
  (ResponseType r ~ a, KnownSymbol desc) =>
  AsUnion
    '[ErrorDescription s label desc, r]
    (Maybe a)
  where
  toUnion Nothing = Z (I mkErrorDescription)
  toUnion (Just x) = S (Z (I x))
  fromUnion (Z (I _)) = Nothing
  fromUnion (S (Z (I x))) = Just x
  fromUnion (S (S x)) = case x of

-- * Empty errors for legacy reasons

data EmptyErrorForLegacyReasons s desc

type instance ResponseType (EmptyErrorForLegacyReasons s desc) = ()

instance
  KnownStatus s =>
  IsResponse cs (EmptyErrorForLegacyReasons s desc)
  where
  type ResponseStatus (EmptyErrorForLegacyReasons s desc) = s

  responseRender _ () =
    pure $
      roAddContentType
        (contentType (Proxy @PlainText))
        (RenderOutput (statusVal (Proxy @s)) mempty mempty)

  responseUnrender _ output =
    guard
      ( LBS.null (roBody output)
          && roStatus output == statusVal (Proxy @s)
      )

instance
  (KnownStatus s, KnownSymbol desc) =>
  IsSwaggerResponse (EmptyErrorForLegacyReasons s desc)
  where
  responseSwagger =
    pure $
      mempty
        & S.description
          .~ ( Text.pack (symbolVal (Proxy @desc))
                 <> "(**Note**: This error has an empty body for legacy reasons)"
             )

instance
  ( ResponseType r ~ a,
    KnownStatus s,
    KnownSymbol desc
  ) =>
  AsUnion
    '[EmptyErrorForLegacyReasons s desc, r]
    (Maybe a)
  where
  toUnion Nothing = Z (I ())
  toUnion (Just x) = S (Z (I x))
  fromUnion (Z (I ())) = Nothing
  fromUnion (S (Z (I x))) = Just x
  fromUnion (S (S x)) = case x of

-- * Errors

mkErrorDescription :: forall code label desc. KnownSymbol desc => ErrorDescription code label desc
mkErrorDescription = ErrorDescription $ Text.pack (symbolVal (Proxy @desc))

type ConvNotFound = ErrorDescription 404 "no-conversation" "Conversation not found"

type ConvMemberNotFound = ErrorDescription 404 "no-conversation-member" "Conversation member not found"

type UnknownClient = ErrorDescription 403 "unknown-client" "Unknown Client"

type ClientNotFound = ErrorDescription 404 "client-not-found" "Client not found"

type NotConnected = ErrorDescription 403 "not-connected" "Users are not connected"

type ConnectionLimitReached = ErrorDescription 403 "connection-limit" "Too many sent/accepted connections."

type InvalidUser = ErrorDescription 400 "invalid-user" "Invalid user."

type InvalidCode = ErrorDescription 403 "invalid-code" "Invalid verification code"

type InvalidTransition = ErrorDescription 403 "bad-conn-update" "Invalid status transition."

type NoIdentity = ErrorDescription 403 "no-identity" "The user has no verified identity (email or phone number)."

noIdentity :: forall code lbl desc. (NoIdentity ~ ErrorDescription code lbl desc) => Int -> NoIdentity
noIdentity n = ErrorDescription (Text.pack (symbolVal (Proxy @desc)) <> " (code " <> Text.pack (show n) <> ")")

type OperationDenied = ErrorDescription 403 "operation-denied" "Insufficient permissions"

operationDenied :: Show perm => perm -> OperationDenied
operationDenied p =
  ErrorDescription $
    "Insufficient permissions (missing " <> Text.pack (show p) <> ")"

type NotATeamMember = ErrorDescription 403 "no-team-member" "Requesting user is not a team member"

type ActionDenied = ErrorDescription 403 "action-denied" "Insufficient authorization"

actionDenied :: Show a => a -> ActionDenied
actionDenied a =
  ErrorDescription $
    "Insufficient authorization (missing " <> Text.pack (show a) <> ")"

type ConvMemberRemovalDenied = ErrorDescription 403 "action-denied" "Insufficient authorization"

type CodeNotFound = ErrorDescription 404 "no-conversation-code" "Conversation code not found"

type ConvAccessDenied = ErrorDescription 403 "access-denied" "Conversation access denied"

type UserNotFound = ErrorDescription 404 "not-found" "User not found"

type ConnectionNotFound = ErrorDescription 404 "not-found" "Connection not found"

connectionNotFound :: ConnectionNotFound
connectionNotFound = mkErrorDescription

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

type BadCredentials =
  ErrorDescription
    403
    "invalid-credentials"
    "Authentication failed."

type DeleteCodePending =
  ErrorDescription
    403
    "pending-delete"
    "A verification code for account deletion is still pending."

deleteCodePending :: DeleteCodePending
deleteCodePending = mkErrorDescription

type OwnerDeletingSelf =
  ErrorDescription
    403
    "no-self-delete-for-team-owner"
    "Team owners are not allowed to delete themselves.  Ask a fellow owner."

type MalformedPrekeys = ErrorDescription 400 "bad-request" "Malformed prekeys uploaded"

malformedPrekeys :: MalformedPrekeys
malformedPrekeys = mkErrorDescription

type MissingLegalholdConsent =
  ErrorDescription
    403
    "missing-legalhold-consent"
    "Failed to connect to a user or to invite a user to a group because somebody \
    \is under legalhold and somebody else has not granted consent."

missingLegalholdConsent :: MissingLegalholdConsent
missingLegalholdConsent = mkErrorDescription

type CustomRolesNotSupported =
  ErrorDescription
    400
    "bad-request"
    "Custom roles not supported"

customRolesNotSupported :: CustomRolesNotSupported
customRolesNotSupported = mkErrorDescription

type InvalidOp desc =
  ErrorDescription
    403
    "invalid-op"
    desc

invalidOpErrorDesc :: KnownSymbol desc => proxy desc -> InvalidOp desc
invalidOpErrorDesc = ErrorDescription . Text.pack . symbolVal

type InvalidOpSelfConv = InvalidOp "invalid operation for self conversation"

invalidOpSelfConv :: InvalidOpSelfConv
invalidOpSelfConv = mkErrorDescription

type InvalidOpOne2OneConv = InvalidOp "invalid operation for 1:1 conversations"

invalidOpOne2OneConv :: InvalidOpOne2OneConv
invalidOpOne2OneConv = mkErrorDescription

type InvalidOpConnectConv = InvalidOp "invalid operation for connect conversation"

invalidOpConnectConv :: InvalidOpConnectConv
invalidOpConnectConv = mkErrorDescription

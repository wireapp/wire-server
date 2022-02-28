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

module Wire.API.ErrorDescription where

import Control.Lens (at, (%~), (.~), (<>~), (?~))
import qualified Data.Aeson as A
import Data.Metrics.Servant
import Data.SOP (I (..), NP (..), NS (..))
import Data.Schema
import Data.Swagger (Swagger)
import qualified Data.Swagger as S
import qualified Data.Swagger.Declare as S
import qualified Data.Text as Text
import GHC.TypeLits (KnownSymbol, Symbol, natVal, symbolVal)
import GHC.TypeNats (Nat)
import Imports hiding (head)
import Network.HTTP.Types as HTTP
import Servant
import Servant.API.Status
import Servant.Client.Core
import Servant.Swagger.Internal
import Wire.API.Routes.MultiVerb
import Wire.API.Team.Permission

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

instance RoutesToPaths api => RoutesToPaths (CanThrow err :> api) where
  getRoutes = getRoutes @api

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
  RespondAs JSON s desc (ErrorDescription s label desc)

type instance ResponseType (ErrorDescription s label desc) = ErrorDescription s label desc

instance
  ( KnownStatus s,
    KnownSymbol label,
    KnownSymbol desc
  ) =>
  IsResponse cs (ErrorDescription s label desc)
  where
  type ResponseStatus (ErrorDescription s label desc) = s
  type ResponseBody (ErrorDescription s label desc) = LByteString

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
  type ResponseBody (EmptyErrorForLegacyReasons s desc) = ()

  responseRender _ () =
    pure $
      addContentType @PlainText
        Response
          { responseStatusCode = statusVal (Proxy @s),
            responseHeaders = mempty,
            responseBody = (),
            responseHttpVersion = HTTP.http11
          }

  responseUnrender _ output = guard (responseStatusCode output == statusVal (Proxy @s))

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

-- * Errors

mkErrorDescription :: forall code label desc. KnownSymbol desc => ErrorDescription code label desc
mkErrorDescription = ErrorDescription $ Text.pack (symbolVal (Proxy @desc))

type ConvNotFound = ErrorDescription 404 "no-conversation" "Conversation not found"

type ConvMemberNotFound = ErrorDescription 404 "no-conversation-member" "Conversation member not found"

type TooManyMembers = ErrorDescription 403 "too-many-members" "Maximum number of members per conversation reached."

type UnknownClient = ErrorDescription 403 "unknown-client" "Unknown Client"

type ClientNotFound = ErrorDescription 404 "client-not-found" "Client not found"

type TeamNotFound = ErrorDescription 404 "no-team" "Team not found"

type NonBindingTeam = ErrorDescription 404 "non-binding-team" "Not member of a binding team"

type NotConnected = ErrorDescription 403 "not-connected" "Users are not connected"

type ConnectionLimitReached = ErrorDescription 403 "connection-limit" "Too many sent/accepted connections."

type InvalidUser = ErrorDescription 400 "invalid-user" "Invalid user."

type InvalidCode = ErrorDescription 403 "invalid-code" "Invalid verification code"

type InvalidTransition = ErrorDescription 403 "bad-conn-update" "Invalid status transition."

type NoIdentity = ErrorDescription 403 "no-identity" "The user has no verified identity (email or phone number)."

noIdentity :: forall code lbl desc. (NoIdentity ~ ErrorDescription code lbl desc) => Int -> NoIdentity
noIdentity n = ErrorDescription (Text.pack (symbolVal (Proxy @desc)) <> " (code " <> Text.pack (show n) <> ")")

type OperationDenied = ErrorDescription 403 "operation-denied" "Insufficient permissions"

-- FUTUREWORK(leif): We need this to document possible (operation denied) errors in the servant routes.
-- Be aware that this is redundant and should be replaced by a more type safe solution in the future.
type family OperationDeniedError (a :: Perm) :: * where
  OperationDeniedError 'SetTeamData = ErrorDescription 403 "operation-denied" "Insufficient permissions (missing SetTeamData)"
  OperationDeniedError 'DeleteTeam = ErrorDescription 403 "operation-denied" "Insufficient permissions (missing DeleteTeam)"

operationDeniedSpecialized :: String -> OperationDenied
operationDeniedSpecialized p =
  ErrorDescription $
    "Insufficient permissions (missing " <> Text.pack p <> ")"

operationDenied :: Show perm => perm -> OperationDenied
operationDenied = operationDeniedSpecialized . show

type NotATeamMember = ErrorDescription 403 "no-team-member" "Requesting user is not a team member"

type Unauthorised = ErrorDescription 403 "unauthorised" "Unauthorised operation"

type ReAuthFailed = ErrorDescription 403 "access-denied" "This operation requires reauthentication"

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

type HandleNotFound = ErrorDescription 404 "not-found" "Handle not found"

type TooManyClients = ErrorDescription 403 "too-many-clients" "Too many clients"

type GuestLinksDisabled = ErrorDescription 409 "guest-links-disabled" "The guest link feature is disabled and all guest links have been revoked."

type MissingAuth =
  ErrorDescription
    403
    "missing-auth"
    "Re-authentication via password required"

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

type OwnerDeletingSelf =
  ErrorDescription
    403
    "no-self-delete-for-team-owner"
    "Team owners are not allowed to delete themselves.  Ask a fellow owner."

type MalformedPrekeys = ErrorDescription 400 "bad-request" "Malformed prekeys uploaded"

type MissingLegalholdConsent =
  ErrorDescription
    403
    "missing-legalhold-consent"
    "Failed to connect to a user or to invite a user to a group because somebody \
    \is under legalhold and somebody else has not granted consent."

type InvalidOp desc =
  ErrorDescription
    403
    "invalid-op"
    desc

type DeleteQueueFull = ErrorDescription 503 "queue-full" "The delete queue is full. No further delete requests can be processed at the moment."

invalidOpErrorDesc :: KnownSymbol desc => proxy desc -> InvalidOp desc
invalidOpErrorDesc = ErrorDescription . Text.pack . symbolVal

type InvalidOpSelfConv = InvalidOp "invalid operation for self conversation"

type InvalidOpOne2OneConv = InvalidOp "invalid operation for 1:1 conversations"

type InvalidOpConnectConv = InvalidOp "invalid operation for connect conversation"

type InvalidTargetAccess = InvalidOp "invalid target access"

type InvalidAccessOp = InvalidOp "invalid operation for conversation without 'code' access"

type AssetTooLarge = ErrorDescription 413 "client-error" "Asset too large"

type InvalidLength = ErrorDescription 400 "invalid-length" "Invalid content length"

type AssetNotFound = ErrorDescription 404 "not-found" "Asset not found"

type NameManagedByScim = ErrorDescription 403 "managed-by-scim" "Updating name is not allowed, because it is managed by SCIM"

type HandleManagedByScim = ErrorDescription 403 "managed-by-scim" "Updating handle is not allowed, because it is managed by SCIM"

type InvalidPhone = ErrorDescription 400 "invalid-phone" "Invalid mobile phone number"

type UserKeyExists = ErrorDescription 409 "key-exists" "The given e-mail address or phone number is in use."

type BlacklistedPhone = ErrorDescription 403 "blacklisted-phone" "The given phone number has been blacklisted due to suspected abuse or a complaint."

type LastIdentity = ErrorDescription 403 "last-identity" "The last user identity (email or phone number) cannot be removed."

type NoPassword = ErrorDescription 403 "no-password" "The user has no password."

type ChangePasswordMustDiffer = ErrorDescription 409 "password-must-differ" "For password change, new and old password must be different."

type HandleExists = ErrorDescription 409 "handle-exists" "The given handle is already taken."

type InvalidHandle = ErrorDescription 400 "invalid-handle" "The given handle is invalid."

type PresenceNotRegistered = ErrorDescription 404 "not-found" "presence not registered"

type ClientGone = ErrorDescription 410 "general" "client gone"

type BroadcastLimitExceeded =
  ErrorDescription
    400
    "too-many-users-to-broadcast"
    "Too many users to fan out the broadcast event to."

type InvalidAction = ErrorDescription 403 "invalid-actions" "The specified actions are invalid."

type PasswordAuthenticationFailed = ErrorDescription 403 "password-authentication-failed" "Password authentication failed."

type CodeAuthenticationFailed = ErrorDescription 403 "code-authentication-failed" "Code authentication failed."

type MLSProtocolError = ErrorDescription 400 "mls-protocol-error" "MLS protocol error"

mlsProtocolError :: Text -> MLSProtocolError
mlsProtocolError = ErrorDescription

type MLSIdentityMismatch =
  ErrorDescription
    403
    "mls-identity-mismatch"
    "Prekey credential does not match qualified client ID"

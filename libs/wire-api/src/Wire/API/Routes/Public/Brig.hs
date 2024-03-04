{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Wire.API.Routes.Public.Brig where

import Control.Lens ((?~))
import Data.Aeson qualified as A (FromJSON, ToJSON, Value)
import Data.ByteString.Conversion
import Data.Code (Timeout)
import Data.CommaSeparatedList (CommaSeparatedList)
import Data.Domain
import Data.Handle
import Data.Id as Id
import Data.Nonce (Nonce)
import Data.OpenApi hiding (Contact, Header, Schema, ToSchema)
import Data.OpenApi qualified as S
import Data.Qualified (Qualified (..))
import Data.Range
import Data.SOP
import Data.Schema as Schema
import Generics.SOP qualified as GSOP
import Imports hiding (head)
import Network.Wai.Utilities
import Servant (JSON)
import Servant hiding (Handler, JSON, addHeader, respond)
import Servant.OpenApi.Internal.Orphans ()
import Wire.API.Call.Config (RTCConfiguration)
import Wire.API.Connection hiding (MissingLegalholdConsent)
import Wire.API.Deprecated
import Wire.API.Error
import Wire.API.Error.Brig
import Wire.API.Error.Empty
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Servant
import Wire.API.MakesFederatedCall
import Wire.API.OAuth
import Wire.API.Properties (PropertyKey, PropertyKeysAndValues, RawPropertyValue)
import Wire.API.Routes.API
import Wire.API.Routes.Bearer
import Wire.API.Routes.Cookies
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.Routes.Public
import Wire.API.Routes.Public.Brig.Bot (BotAPI)
import Wire.API.Routes.Public.Brig.OAuth (OAuthAPI)
import Wire.API.Routes.Public.Brig.Provider (ProviderAPI)
import Wire.API.Routes.Public.Brig.Services (ServicesAPI)
import Wire.API.Routes.Public.Util
import Wire.API.Routes.QualifiedCapture
import Wire.API.Routes.Version
import Wire.API.SystemSettings
import Wire.API.Team.Invitation
import Wire.API.Team.Size
import Wire.API.User hiding (NoIdentity)
import Wire.API.User.Activation
import Wire.API.User.Auth
import Wire.API.User.Client
import Wire.API.User.Client.DPoPAccessToken
import Wire.API.User.Client.Prekey
import Wire.API.User.Handle
import Wire.API.User.Password (CompletePasswordReset, NewPasswordReset, PasswordReset, PasswordResetKey)
import Wire.API.User.RichInfo (RichInfoAssocList)
import Wire.API.User.Search (Contact, PagingState, RoleFilter, SearchResult, TeamContact, TeamUserSearchSortBy, TeamUserSearchSortOrder)
import Wire.API.UserMap

type BrigAPI =
  UserAPI
    :<|> SelfAPI
    :<|> AccountAPI
    :<|> ClientAPI
    :<|> PrekeyAPI
    :<|> UserClientAPI
    :<|> ConnectionAPI
    :<|> PropertiesAPI
    :<|> MLSAPI
    :<|> UserHandleAPI
    :<|> SearchAPI
    :<|> AuthAPI
    :<|> CallingAPI
    :<|> TeamsAPI
    :<|> SystemSettingsAPI
    :<|> OAuthAPI
    :<|> BotAPI
    :<|> ServicesAPI
    :<|> ProviderAPI

data BrigAPITag

instance ServiceAPI BrigAPITag v where
  type ServiceAPIRoutes BrigAPITag = BrigAPI

-------------------------------------------------------------------------------
-- User API

type MaxUsersForListClientsBulk = 500

type GetUserVerb =
  MultiVerb
    'GET
    '[JSON]
    '[ ErrorResponse 'UserNotFound,
       Respond 200 "User found" UserProfile
     ]
    (Maybe UserProfile)

type CaptureUserId name = Capture' '[Description "User Id"] name UserId

type QualifiedCaptureUserId name = QualifiedCapture' '[Description "User Id"] name UserId

type CaptureClientId name = Capture' '[Description "ClientId"] name ClientId

type NewClientResponse = Headers '[Header "Location" ClientId] Client

type DeleteSelfResponses =
  '[ RespondEmpty 200 "Deletion is initiated.",
     RespondWithDeletionCodeTimeout
   ]

newtype RespondWithDeletionCodeTimeout
  = RespondWithDeletionCodeTimeout
      (Respond 202 "Deletion is pending verification with a code." DeletionCodeTimeout)
  deriving (IsResponse '[JSON], IsSwaggerResponse)

type instance ResponseType RespondWithDeletionCodeTimeout = DeletionCodeTimeout

instance AsUnion DeleteSelfResponses (Maybe Timeout) where
  toUnion (Just t) = S (Z (I (DeletionCodeTimeout t)))
  toUnion Nothing = Z (I ())
  fromUnion (Z (I ())) = Nothing
  fromUnion (S (Z (I (DeletionCodeTimeout t)))) = Just t
  fromUnion (S (S x)) = case x of {}

type ConnectionUpdateResponses = UpdateResponses "Connection unchanged" "Connection updated" UserConnection

type UserAPI =
  -- See Note [ephemeral user sideeffect]
  Named
    "get-user-unqualified"
    ( Summary "Get a user by UserId"
        :> MakesFederatedCall 'Brig "get-users-by-ids"
        :> Until 'V2
        :> ZUser
        :> "users"
        :> CaptureUserId "uid"
        :> GetUserVerb
    )
    :<|>
    -- See Note [ephemeral user sideeffect]
    Named
      "get-user-qualified"
      ( Summary "Get a user by Domain and UserId"
          :> MakesFederatedCall 'Brig "get-users-by-ids"
          :> ZUser
          :> "users"
          :> QualifiedCaptureUserId "uid"
          :> GetUserVerb
      )
    :<|> Named
           "update-user-email"
           ( Summary "Resend email address validation email."
               :> Description "If the user has a pending email validation, the validation email will be resent."
               :> ZUser
               :> "users"
               :> CaptureUserId "uid"
               :> "email"
               :> ReqBody '[JSON] EmailUpdate
               :> Put '[JSON] ()
           )
    :<|> Named
           "get-handle-info-unqualified"
           ( Summary "(deprecated, use /search/contacts) Get information on a user handle"
               :> Until 'V2
               :> MakesFederatedCall 'Brig "get-user-by-handle"
               :> MakesFederatedCall 'Brig "get-users-by-ids"
               :> ZUser
               :> "users"
               :> "handles"
               :> Capture' '[Description "The user handle"] "handle" Handle
               :> MultiVerb
                    'GET
                    '[JSON]
                    '[ ErrorResponse 'HandleNotFound,
                       Respond 200 "User found" UserHandleInfo
                     ]
                    (Maybe UserHandleInfo)
           )
    :<|> Named
           "get-user-by-handle-qualified"
           ( Summary "(deprecated, use /search/contacts) Get information on a user handle"
               :> Until 'V2
               :> MakesFederatedCall 'Brig "get-user-by-handle"
               :> MakesFederatedCall 'Brig "get-users-by-ids"
               :> ZUser
               :> "users"
               :> "by-handle"
               :> QualifiedCapture' '[Description "The user handle"] "handle" Handle
               :> MultiVerb
                    'GET
                    '[JSON]
                    '[ ErrorResponse 'HandleNotFound,
                       Respond 200 "User found" UserProfile
                     ]
                    (Maybe UserProfile)
           )
    :<|>
    -- See Note [ephemeral user sideeffect]
    Named
      "list-users-by-unqualified-ids-or-handles"
      ( Summary "List users (deprecated)"
          :> Until 'V2
          :> Description "The 'ids' and 'handles' parameters are mutually exclusive."
          :> MakesFederatedCall 'Brig "get-users-by-ids"
          :> ZUser
          :> "users"
          :> QueryParam' [Optional, Strict, Description "User IDs of users to fetch"] "ids" (CommaSeparatedList UserId)
          :> QueryParam' [Optional, Strict, Description "Handles of users to fetch, min 1 and max 4 (the check for handles is rather expensive)"] "handles" (Range 1 4 (CommaSeparatedList Handle))
          :> Get '[JSON] [UserProfile]
      )
    :<|> Named
           "list-users-by-ids-or-handles"
           ( Summary "List users"
               :> Description "The 'qualified_ids' and 'qualified_handles' parameters are mutually exclusive."
               :> MakesFederatedCall 'Brig "get-users-by-ids"
               :> ZUser
               :> From 'V4
               :> "list-users"
               :> ReqBody '[JSON] ListUsersQuery
               :> Post '[JSON] ListUsersById
           )
    :<|>
    -- See Note [ephemeral user sideeffect]
    Named
      "list-users-by-ids-or-handles@V3"
      ( Summary "List users"
          :> Description "The 'qualified_ids' and 'qualified_handles' parameters are mutually exclusive."
          :> MakesFederatedCall 'Brig "get-users-by-ids"
          :> ZUser
          :> Until 'V4
          :> "list-users"
          :> ReqBody '[JSON] ListUsersQuery
          :> Post '[JSON] [UserProfile]
      )
    :<|> Named
           "send-verification-code"
           ( Summary "Send a verification code to a given email address."
               :> "verification-code"
               :> "send"
               :> ReqBody '[JSON] SendVerificationCode
               :> MultiVerb 'POST '[JSON] '[RespondEmpty 200 "Verification code sent."] ()
           )
    :<|> Named
           "get-rich-info"
           ( Summary "Get a user's rich info"
               :> CanThrow 'InsufficientTeamPermissions
               :> ZUser
               :> "users"
               :> CaptureUserId "uid"
               :> "rich-info"
               :> MultiVerb
                    'GET
                    '[JSON]
                    '[Respond 200 "Rich info about the user" RichInfoAssocList]
                    RichInfoAssocList
           )
    :<|> Named
           "get-supported-protocols"
           ( Summary "Get a user's supported protocols"
               :> From 'V5
               :> ZLocalUser
               :> "users"
               :> QualifiedCaptureUserId "uid"
               :> "supported-protocols"
               :> MultiVerb1
                    'GET
                    '[JSON]
                    (Respond 200 "Protocols supported by the user" (Set BaseProtocolTag))
           )

type SelfAPI =
  Named
    "get-self"
    ( Summary "Get your own profile"
        :> DescriptionOAuthScope 'ReadSelf
        :> ZUser
        :> "self"
        :> Get '[JSON] SelfProfile
    )
    :<|>
    -- This endpoint can lead to the following events being sent:
    -- - UserDeleted event to contacts of self
    -- - MemberLeave event to members for all conversations the user was in (via galley)
    Named
      "delete-self"
      ( Summary "Initiate account deletion."
          :> Description
               "if the account has a verified identity, a verification \
               \code is sent and needs to be confirmed to authorise the \
               \deletion. if the account has no verified identity but a \
               \password, it must be provided. if password is correct, or if neither \
               \a verified identity nor a password exists, account deletion \
               \is scheduled immediately."
          :> MakesFederatedCall 'Brig "send-connection-action"
          :> CanThrow 'InvalidUser
          :> CanThrow 'InvalidCode
          :> CanThrow 'BadCredentials
          :> CanThrow 'MissingAuth
          :> CanThrow 'DeleteCodePending
          :> CanThrow 'OwnerDeletingSelf
          :> ZUser
          :> "self"
          :> ReqBody '[JSON] DeleteUser
          :> MultiVerb 'DELETE '[JSON] DeleteSelfResponses (Maybe Timeout)
      )
    :<|>
    -- This endpoint can lead to the following events being sent:
    -- - UserUpdated event to contacts of self
    Named
      "put-self"
      ( Summary "Update your profile."
          :> MakesFederatedCall 'Brig "send-connection-action"
          :> ZUser
          :> ZConn
          :> "self"
          :> ReqBody '[JSON] UserUpdate
          :> MultiVerb 'PUT '[JSON] PutSelfResponses (Maybe UpdateProfileError)
      )
    :<|> Named
           "change-phone"
           ( Summary "Change your phone number."
               :> ZUser
               :> ZConn
               :> "self"
               :> "phone"
               :> ReqBody '[JSON] PhoneUpdate
               :> MultiVerb 'PUT '[JSON] ChangePhoneResponses (Maybe ChangePhoneError)
           )
    :<|>
    -- This endpoint can lead to the following events being sent:
    -- - UserIdentityRemoved event to self
    Named
      "remove-phone"
      ( Summary "Remove your phone number."
          :> Description
               "Your phone number can only be removed if you also have an \
               \email address and a password."
          :> MakesFederatedCall 'Brig "send-connection-action"
          :> ZUser
          :> ZConn
          :> "self"
          :> "phone"
          :> MultiVerb 'DELETE '[JSON] RemoveIdentityResponses (Maybe RemoveIdentityError)
      )
    :<|>
    -- This endpoint can lead to the following events being sent:
    -- - UserIdentityRemoved event to self
    Named
      "remove-email"
      ( Summary "Remove your email address."
          :> Description
               "Your email address can only be removed if you also have a \
               \phone number."
          :> MakesFederatedCall 'Brig "send-connection-action"
          :> ZUser
          :> ZConn
          :> "self"
          :> "email"
          :> MultiVerb 'DELETE '[JSON] RemoveIdentityResponses (Maybe RemoveIdentityError)
      )
    :<|> Named
           "check-password-exists"
           ( Summary "Check that your password is set."
               :> ZUser
               :> "self"
               :> "password"
               :> MultiVerb
                    'HEAD
                    '()
                    '[ RespondEmpty 404 "Password is not set",
                       RespondEmpty 200 "Password is set"
                     ]
                    Bool
           )
    :<|> Named
           "change-password"
           ( Summary "Change your password."
               :> ZUser
               :> "self"
               :> "password"
               :> ReqBody '[JSON] PasswordChange
               :> MultiVerb 'PUT '[JSON] ChangePasswordResponses (Maybe ChangePasswordError)
           )
    :<|> Named
           "change-locale"
           ( Summary "Change your locale."
               :> MakesFederatedCall 'Brig "send-connection-action"
               :> ZUser
               :> ZConn
               :> "self"
               :> "locale"
               :> ReqBody '[JSON] LocaleUpdate
               :> MultiVerb 'PUT '[JSON] '[RespondEmpty 200 "Local Changed"] ()
           )
    :<|> Named
           "change-handle"
           ( Summary "Change your handle."
               :> MakesFederatedCall 'Brig "send-connection-action"
               :> MakesFederatedCall 'Brig "send-connection-action"
               :> ZUser
               :> ZConn
               :> "self"
               :> "handle"
               :> ReqBody '[JSON] HandleUpdate
               :> MultiVerb 'PUT '[JSON] ChangeHandleResponses (Maybe ChangeHandleError)
           )
    :<|> Named
           "change-supported-protocols"
           ( Summary "Change your supported protocols"
               :> From 'V5
               :> ZLocalUser
               :> ZConn
               :> "self"
               :> "supported-protocols"
               :> ReqBody '[JSON] SupportedProtocolUpdate
               :> MultiVerb1 'PUT '[JSON] (RespondEmpty 200 "Supported protocols changed")
           )

type UserHandleAPI =
  Named
    "check-user-handles"
    ( Summary "Check availability of user handles"
        :> ZUser
        :> "users"
        :> "handles"
        :> ReqBody '[JSON] CheckHandles
        :> MultiVerb
             'POST
             '[JSON]
             '[Respond 200 "List of free handles" [Handle]]
             [Handle]
    )
    :<|> Named
           "check-user-handle"
           ( Summary "Check whether a user handle can be taken"
               :> CanThrow 'InvalidHandle
               :> CanThrow 'HandleNotFound
               :> ZUser
               :> "users"
               :> "handles"
               :> Capture "handle" Text
               :> MultiVerb
                    'HEAD
                    '[JSON]
                    '[Respond 200 "Handle is taken" ()]
                    ()
           )

type AccountAPI =
  -- docs/reference/user/registration.md {#RefRegistration}
  --
  -- This endpoint can lead to the following events being sent:
  -- - UserActivated event to created user, if it is a team invitation or user has an SSO ID
  -- - UserIdentityUpdated event to created user, if email code or phone code is provided
  Named
    "register"
    ( Summary "Register a new user."
        :> Description
             "If the environment where the registration takes \
             \place is private and a registered email address or phone \
             \number is not whitelisted, a 403 error is returned."
        :> MakesFederatedCall 'Brig "send-connection-action"
        :> "register"
        :> ReqBody '[JSON] NewUserPublic
        :> MultiVerb 'POST '[JSON] RegisterResponses (Either RegisterError RegisterSuccess)
    )
    -- This endpoint can lead to the following events being sent:
    -- UserDeleted event to contacts of deleted user
    -- MemberLeave event to members for all conversations the user was in (via galley)
    :<|> Named
           "verify-delete"
           ( Summary "Verify account deletion with a code."
               :> MakesFederatedCall 'Brig "send-connection-action"
               :> CanThrow 'InvalidCode
               :> "delete"
               :> ReqBody '[JSON] VerifyDeleteUser
               :> MultiVerb 'POST '[JSON] '[RespondEmpty 200 "Deletion is initiated."] ()
           )
    -- This endpoint can lead to the following events being sent:
    -- - UserActivated event to the user, if account gets activated
    -- - UserIdentityUpdated event to the user, if email or phone get activated
    :<|> Named
           "get-activate"
           ( Summary "Activate (i.e. confirm) an email address or phone number."
               :> MakesFederatedCall 'Brig "send-connection-action"
               :> Description "See also 'POST /activate' which has a larger feature set."
               :> CanThrow 'UserKeyExists
               :> CanThrow 'InvalidActivationCodeWrongUser
               :> CanThrow 'InvalidActivationCodeWrongCode
               :> CanThrow 'InvalidEmail
               :> CanThrow 'InvalidPhone
               :> "activate"
               :> QueryParam' '[Required, Strict, Description "Activation key"] "key" ActivationKey
               :> QueryParam' '[Required, Strict, Description "Activation code"] "code" ActivationCode
               :> MultiVerb
                    'GET
                    '[JSON]
                    GetActivateResponse
                    ActivationRespWithStatus
           )
    -- docs/reference/user/activation.md {#RefActivationSubmit}
    --
    -- This endpoint can lead to the following events being sent:
    -- - UserActivated event to the user, if account gets activated
    -- - UserIdentityUpdated event to the user, if email or phone get activated
    :<|> Named
           "post-activate"
           ( Summary "Activate (i.e. confirm) an email address or phone number."
               :> Description
                    "Activation only succeeds once and the number of \
                    \failed attempts for a valid key is limited."
               :> MakesFederatedCall 'Brig "send-connection-action"
               :> CanThrow 'UserKeyExists
               :> CanThrow 'InvalidActivationCodeWrongUser
               :> CanThrow 'InvalidActivationCodeWrongCode
               :> CanThrow 'InvalidEmail
               :> CanThrow 'InvalidPhone
               :> "activate"
               :> ReqBody '[JSON] Activate
               :> MultiVerb
                    'POST
                    '[JSON]
                    GetActivateResponse
                    ActivationRespWithStatus
           )
    -- docs/reference/user/activation.md {#RefActivationRequest}
    :<|> Named
           "post-activate-send"
           ( Summary "Send (or resend) an email or phone activation code."
               :> CanThrow 'UserKeyExists
               :> CanThrow 'InvalidEmail
               :> CanThrow 'InvalidPhone
               :> CanThrow 'BlacklistedEmail
               :> CanThrow 'BlacklistedPhone
               :> CanThrow 'CustomerExtensionBlockedDomain
               :> "activate"
               :> "send"
               :> ReqBody '[JSON] SendActivationCode
               :> MultiVerb 'POST '[JSON] '[RespondEmpty 200 "Activation code sent."] ()
           )
    :<|> Named
           "post-password-reset"
           ( Summary "Initiate a password reset."
               :> CanThrow 'PasswordResetInProgress
               :> CanThrow 'InvalidPasswordResetKey
               :> "password-reset"
               :> ReqBody '[JSON] NewPasswordReset
               :> MultiVerb 'POST '[JSON] '[RespondEmpty 201 "Password reset code created and sent by email."] ()
           )
    :<|> Named
           "post-password-reset-complete"
           ( Summary "Complete a password reset."
               :> CanThrow 'InvalidPasswordResetCode
               :> "password-reset"
               :> "complete"
               :> ReqBody '[JSON] CompletePasswordReset
               :> MultiVerb 'POST '[JSON] '[RespondEmpty 200 "Password reset successful."] ()
           )
    :<|> Named
           "post-password-reset-key-deprecated"
           ( Summary "Complete a password reset."
               :> Deprecated
               :> CanThrow 'PasswordResetInProgress
               :> CanThrow 'InvalidPasswordResetKey
               :> CanThrow 'InvalidPasswordResetCode
               :> CanThrow 'ResetPasswordMustDiffer
               :> Description "DEPRECATED: Use 'POST /password-reset/complete'."
               :> "password-reset"
               :> Capture' '[Description "An opaque key for a pending password reset."] "key" PasswordResetKey
               :> ReqBody '[JSON] PasswordReset
               :> MultiVerb 'POST '[JSON] '[RespondEmpty 200 "Password reset successful."] ()
           )
    :<|> Named
           "onboarding"
           ( Summary "Upload contacts and invoke matching."
               :> Deprecated
               :> Description
                    "DEPRECATED: the feature has been turned off, the end-point does \
                    \nothing and always returns '{\"results\":[],\"auto-connects\":[]}'."
               :> ZUser
               :> "onboarding"
               :> "v3"
               :> ReqBody '[JSON] JsonValue
               :> Post '[JSON] DeprecatedMatchingResult
           )

newtype JsonValue = JsonValue {fromJsonValue :: A.Value}
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema JsonValue)

instance ToSchema JsonValue where
  schema = fromJsonValue .= (JsonValue <$> named "Body" jsonValue)

data DeprecatedMatchingResult = DeprecatedMatchingResult
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema DeprecatedMatchingResult)

instance ToSchema DeprecatedMatchingResult where
  schema =
    objectWithDocModifier
      "DeprecatedMatchingResult"
      (S.deprecated ?~ True)
      $ DeprecatedMatchingResult
        <$ const []
          .= field "results" (array (null_ @SwaggerDoc))
        <* const []
          .= field "auto-connects" (array (null_ @SwaggerDoc))

data ActivationRespWithStatus
  = ActivationResp ActivationResponse
  | ActivationRespDryRun
  | ActivationRespPass
  | ActivationRespSuccessNoIdent
  deriving (Generic)
  deriving (AsUnion GetActivateResponse) via GenericAsUnion GetActivateResponse ActivationRespWithStatus

instance GSOP.Generic ActivationRespWithStatus

type GetActivateResponse =
  '[ Respond 200 "Activation successful." ActivationResponse,
     RespondEmpty 200 "Activation successful. (Dry run)",
     RespondEmpty 204 "A recent activation was already successful.",
     RespondEmpty 200 "Activation successful."
   ]

type PrekeyAPI =
  Named
    "get-users-prekeys-client-unqualified"
    ( Summary "(deprecated) Get a prekey for a specific client of a user."
        :> Until 'V2
        :> MakesFederatedCall 'Brig "claim-prekey"
        :> ZUser
        :> "users"
        :> CaptureUserId "uid"
        :> "prekeys"
        :> CaptureClientId "client"
        :> Get '[JSON] ClientPrekey
    )
    :<|> Named
           "get-users-prekeys-client-qualified"
           ( Summary "Get a prekey for a specific client of a user."
               :> MakesFederatedCall 'Brig "claim-prekey"
               :> ZUser
               :> "users"
               :> QualifiedCaptureUserId "uid"
               :> "prekeys"
               :> CaptureClientId "client"
               :> Get '[JSON] ClientPrekey
           )
    :<|> Named
           "get-users-prekey-bundle-unqualified"
           ( Summary "(deprecated) Get a prekey for each client of a user."
               :> Until 'V2
               :> MakesFederatedCall 'Brig "claim-prekey-bundle"
               :> ZUser
               :> "users"
               :> CaptureUserId "uid"
               :> "prekeys"
               :> Get '[JSON] PrekeyBundle
           )
    :<|> Named
           "get-users-prekey-bundle-qualified"
           ( Summary "Get a prekey for each client of a user."
               :> MakesFederatedCall 'Brig "claim-prekey-bundle"
               :> ZUser
               :> "users"
               :> QualifiedCaptureUserId "uid"
               :> "prekeys"
               :> Get '[JSON] PrekeyBundle
           )
    :<|> Named
           "get-multi-user-prekey-bundle-unqualified"
           ( Summary
               "(deprecated)  Given a map of user IDs to client IDs return a prekey for each one."
               :> Description "You can't request information for more users than maximum conversation size."
               :> Until 'V2
               :> ZUser
               :> "users"
               :> "prekeys"
               :> ReqBody '[JSON] UserClients
               :> Post '[JSON] UserClientPrekeyMap
           )
    :<|> Named
           "get-multi-user-prekey-bundle-qualified@v3"
           ( Summary
               "(deprecated)  Given a map of user IDs to client IDs return a prekey for each one."
               :> Description "You can't request information for more users than maximum conversation size."
               :> MakesFederatedCall 'Brig "claim-multi-prekey-bundle"
               :> ZUser
               :> Until 'V4
               :> "users"
               :> "list-prekeys"
               :> ReqBody '[JSON] QualifiedUserClients
               :> Post '[JSON] QualifiedUserClientPrekeyMap
           )
    :<|> Named
           "get-multi-user-prekey-bundle-qualified"
           ( Summary
               "(deprecated)  Given a map of user IDs to client IDs return a prekey for each one."
               :> Description "You can't request information for more users than maximum conversation size."
               :> MakesFederatedCall 'Brig "claim-multi-prekey-bundle"
               :> ZUser
               :> From 'V4
               :> "users"
               :> "list-prekeys"
               :> ReqBody '[JSON] QualifiedUserClients
               :> Post '[JSON] QualifiedUserClientPrekeyMapV4
           )

type UserClientAPI =
  -- User Client API ----------------------------------------------------

  -- This endpoint can lead to the following events being sent:
  -- - ClientAdded event to self
  -- - ClientRemoved event to self, if removing old clients due to max number
  Named
    "add-client"
    ( Summary "Register a new client"
        :> MakesFederatedCall 'Brig "send-connection-action"
        :> CanThrow 'TooManyClients
        :> CanThrow 'MissingAuth
        :> CanThrow 'MalformedPrekeys
        :> CanThrow 'CodeAuthenticationFailed
        :> CanThrow 'CodeAuthenticationRequired
        :> ZUser
        :> ZConn
        :> "clients"
        :> ReqBody '[JSON] NewClient
        :> Verb 'POST 201 '[JSON] NewClientResponse
    )
    :<|> Named
           "update-client"
           ( Summary "Update a registered client"
               :> CanThrow 'MalformedPrekeys
               :> ZUser
               :> "clients"
               :> CaptureClientId "client"
               :> ReqBody '[JSON] UpdateClient
               :> MultiVerb 'PUT '[JSON] '[RespondEmpty 200 "Client updated"] ()
           )
    :<|>
    -- This endpoint can lead to the following events being sent:
    -- - ClientRemoved event to self
    Named
      "delete-client"
      ( Summary "Delete an existing client"
          :> ZUser
          :> ZConn
          :> "clients"
          :> CaptureClientId "client"
          :> ReqBody '[JSON] RmClient
          :> MultiVerb 'DELETE '[JSON] '[RespondEmpty 200 "Client deleted"] ()
      )
    :<|> Named
           "list-clients"
           ( Summary "List the registered clients"
               :> ZUser
               :> "clients"
               :> Get '[JSON] [Client]
           )
    :<|> Named
           "get-client"
           ( Summary "Get a registered client by ID"
               :> ZUser
               :> "clients"
               :> CaptureClientId "client"
               :> MultiVerb
                    'GET
                    '[JSON]
                    '[ EmptyErrorForLegacyReasons 404 "Client not found",
                       Respond 200 "Client found" Client
                     ]
                    (Maybe Client)
           )
    :<|> Named
           "get-client-capabilities"
           ( Summary "Read back what the client has been posting about itself"
               :> ZUser
               :> "clients"
               :> CaptureClientId "client"
               :> "capabilities"
               :> Get '[JSON] ClientCapabilityList
           )
    :<|> Named
           "get-client-prekeys"
           ( Summary "List the remaining prekey IDs of a client"
               :> ZUser
               :> "clients"
               :> CaptureClientId "client"
               :> "prekeys"
               :> Get '[JSON] [PrekeyId]
           )
    -- be aware that the order of the head-nonce and get-nonce matters, if get was first, then head requests would be routed to the get handler
    :<|> NewNonce "head-nonce" 'HEAD 200
    :<|> NewNonce "get-nonce" 'GET 204
    :<|> CreateAccessToken

type CreateAccessToken =
  Named
    "create-access-token"
    ( Summary "Create a JWT DPoP access token"
        :> Description
             ( "Create an JWT DPoP access token for the client CSR, given a JWT DPoP proof, specified in the `DPoP` header. \
               \The access token will be returned as JWT DPoP token in the `DPoP` header."
             )
        :> ZLocalUser
        :> "clients"
        :> CaptureClientId "cid"
        :> "access-token"
        :> Header' '[Required, Strict] "DPoP" Proof
        :> MultiVerb1
             'POST
             '[JSON]
             ( WithHeaders
                 '[Header "Cache-Control" CacheControl]
                 (DPoPAccessTokenResponse, CacheControl)
                 (Respond 200 "Access token created" DPoPAccessTokenResponse)
             )
    )

type NewNonce name method statusCode =
  Named
    name
    ( Summary "Get a new nonce for a client CSR"
        :> Description "Get a new nonce for a client CSR, specified in the response header `Replay-Nonce` as a uuidv4 in base64url encoding."
        :> ZUser
        :> "clients"
        :> CaptureClientId "client"
        :> "nonce"
        :> MultiVerb1
             method
             '[JSON]
             ( WithHeaders
                 '[Header "Replay-Nonce" NonceHeader, Header "Cache-Control" CacheControl]
                 (Nonce, CacheControl)
                 (RespondEmpty statusCode "No Content")
             )
    )

newtype NonceHeader = NonceHeader Nonce
  deriving (Eq, Show)
  deriving newtype (FromByteString, ToByteString, ToParamSchema, ToHttpApiData, FromHttpApiData)

instance AsHeaders '[NonceHeader, CacheControl] () (Nonce, CacheControl) where
  fromHeaders (I (NonceHeader n) :* (I cc :* Nil), ()) = (n, cc)
  toHeaders (n, cc) = (I (NonceHeader n) :* (I cc :* Nil), ())

type ClientAPI =
  Named
    "get-user-clients-unqualified"
    ( Summary "Get all of a user's clients"
        :> Until 'V2
        :> MakesFederatedCall 'Brig "get-user-clients"
        :> "users"
        :> CaptureUserId "uid"
        :> "clients"
        :> Get '[JSON] [PubClient]
    )
    :<|> Named
           "get-user-clients-qualified"
           ( Summary "Get all of a user's clients"
               :> MakesFederatedCall 'Brig "get-user-clients"
               :> "users"
               :> QualifiedCaptureUserId "uid"
               :> "clients"
               :> Get '[JSON] [PubClient]
           )
    :<|> Named
           "get-user-client-unqualified"
           ( Summary "Get a specific client of a user"
               :> Until 'V2
               :> MakesFederatedCall 'Brig "get-user-clients"
               :> "users"
               :> CaptureUserId "uid"
               :> "clients"
               :> CaptureClientId "client"
               :> Get '[JSON] PubClient
           )
    :<|> Named
           "get-user-client-qualified"
           ( Summary "Get a specific client of a user"
               :> MakesFederatedCall 'Brig "get-user-clients"
               :> "users"
               :> QualifiedCaptureUserId "uid"
               :> "clients"
               :> CaptureClientId "client"
               :> Get '[JSON] PubClient
           )
    :<|> Named
           "list-clients-bulk"
           ( Summary "List all clients for a set of user ids"
               :> Until 'V2
               :> MakesFederatedCall 'Brig "get-user-clients"
               :> ZUser
               :> "users"
               :> "list-clients"
               :> ReqBody '[JSON] (Range 1 MaxUsersForListClientsBulk [Qualified UserId])
               :> Post '[JSON] (QualifiedUserMap (Set PubClient))
           )
    :<|> Named
           "list-clients-bulk-v2"
           ( Summary "List all clients for a set of user ids"
               :> Until 'V2
               :> MakesFederatedCall 'Brig "get-user-clients"
               :> ZUser
               :> "users"
               :> "list-clients"
               :> "v2"
               :> ReqBody '[JSON] (LimitedQualifiedUserIdList MaxUsersForListClientsBulk)
               :> Post '[JSON] (WrappedQualifiedUserMap (Set PubClient))
           )
    :<|> Named
           "list-clients-bulk@v2"
           ( Summary "List all clients for a set of user ids"
               :> Description "If a backend is unreachable, the clients from that backend will be omitted from the response"
               :> From 'V2
               :> MakesFederatedCall 'Brig "get-user-clients"
               :> ZUser
               :> "users"
               :> "list-clients"
               :> ReqBody '[JSON] (LimitedQualifiedUserIdList MaxUsersForListClientsBulk)
               :> Post '[JSON] (WrappedQualifiedUserMap (Set PubClient))
           )

-- Connection API -----------------------------------------------------
--
-- This endpoint can lead to the following events being sent:
-- - ConnectionUpdated event to self and other, if any side's connection state changes
-- - MemberJoin event to self and other, if joining an existing connect conversation (via galley)
-- - ConvCreate event to self, if creating a connect conversation (via galley)
-- - ConvConnect event to self, in some cases (via galley),
--   for details see 'Galley.API.Create.createConnectConversation'
type ConnectionAPI =
  Named
    "create-connection-unqualified"
    ( Summary "Create a connection to another user"
        :> Until 'V2
        :> MakesFederatedCall 'Brig "send-connection-action"
        :> CanThrow 'MissingLegalholdConsentOldClients
        :> CanThrow 'MissingLegalholdConsent
        :> CanThrow 'InvalidUser
        :> CanThrow 'ConnectionLimitReached
        :> CanThrow 'NoIdentity
        -- Config value 'setUserMaxConnections' value in production/by default
        -- is currently 1000 and has not changed in the last few years.
        -- While it would be more correct to use the config value here, that
        -- might not be time well spent.
        :> Description "You can have no more than 1000 connections in accepted or sent state"
        :> ZUser
        :> ZConn
        :> "connections"
        :> ReqBody '[JSON] ConnectionRequest
        :> MultiVerb
             'POST
             '[JSON]
             (ResponsesForExistedCreated "Connection existed" "Connection was created" UserConnection)
             (ResponseForExistedCreated UserConnection)
    )
    :<|> Named
           "create-connection"
           ( Summary "Create a connection to another user"
               :> MakesFederatedCall 'Brig "get-users-by-ids"
               :> MakesFederatedCall 'Brig "send-connection-action"
               :> CanThrow 'MissingLegalholdConsentOldClients
               :> CanThrow 'MissingLegalholdConsent
               :> CanThrow 'InvalidUser
               :> CanThrow 'ConnectionLimitReached
               :> CanThrow 'NoIdentity
               -- Config value 'setUserMaxConnections' value in production/by default
               -- is currently 1000 and has not changed in the last few years.
               -- While it would be more correct to use the config value here, that
               -- might not be time well spent.
               :> Description "You can have no more than 1000 connections in accepted or sent state"
               :> ZUser
               :> ZConn
               :> "connections"
               :> QualifiedCaptureUserId "uid"
               :> MultiVerb
                    'POST
                    '[JSON]
                    (ResponsesForExistedCreated "Connection existed" "Connection was created" UserConnection)
                    (ResponseForExistedCreated UserConnection)
           )
    :<|> Named
           "list-local-connections"
           ( Summary "List the local connections to other users"
               :> Until 'V2
               :> ZUser
               :> "connections"
               :> QueryParam' '[Optional, Strict, Description "User ID to start from when paginating"] "start" UserId
               :> QueryParam' '[Optional, Strict, Description "Number of results to return (default 100, max 500)"] "size" (Range 1 500 Int32)
               :> Get '[JSON] UserConnectionList
           )
    :<|> Named
           "list-connections"
           ( Summary "List the connections to other users, including remote users"
               :> Description PaginationDocs
               :> ZUser
               :> "list-connections"
               :> ReqBody '[JSON] ListConnectionsRequestPaginated
               :> Post '[JSON] ConnectionsPage
           )
    :<|> Named
           "get-connection-unqualified"
           ( Summary "Get an existing connection to another user"
               :> Until 'V2
               :> ZUser
               :> "connections"
               :> CaptureUserId "uid"
               :> MultiVerb
                    'GET
                    '[JSON]
                    '[ EmptyErrorForLegacyReasons 404 "Connection not found",
                       Respond 200 "Connection found" UserConnection
                     ]
                    (Maybe UserConnection)
           )
    :<|> Named
           "get-connection"
           ( Summary "Get an existing connection to another user (local or remote)"
               :> ZUser
               :> "connections"
               :> QualifiedCaptureUserId "uid"
               :> MultiVerb
                    'GET
                    '[JSON]
                    '[ EmptyErrorForLegacyReasons 404 "Connection not found",
                       Respond 200 "Connection found" UserConnection
                     ]
                    (Maybe UserConnection)
           )
    :<|>
    -- This endpoint can lead to the following events being sent:
    -- - ConnectionUpdated event to self and other, if their connection states change
    --
    -- When changing the connection state to Sent or Accepted, this can cause events to be sent
    -- when joining the connect conversation:
    -- - MemberJoin event to self and other (via galley)
    Named
      "update-connection-unqualified"
      ( Summary "Update a connection to another user"
          :> Until 'V2
          :> MakesFederatedCall 'Brig "send-connection-action"
          :> CanThrow 'MissingLegalholdConsentOldClients
          :> CanThrow 'MissingLegalholdConsent
          :> CanThrow 'InvalidUser
          :> CanThrow 'ConnectionLimitReached
          :> CanThrow 'NotConnected
          :> CanThrow 'InvalidTransition
          :> CanThrow 'NoIdentity
          :> ZUser
          :> ZConn
          :> "connections"
          :> CaptureUserId "uid"
          :> ReqBody '[JSON] ConnectionUpdate
          :> MultiVerb
               'PUT
               '[JSON]
               ConnectionUpdateResponses
               (UpdateResult UserConnection)
      )
    :<|>
    -- This endpoint can lead to the following events being sent:
    -- - ConnectionUpdated event to self and other, if their connection states change
    --
    -- When changing the connection state to Sent or Accepted, this can cause events to be sent
    -- when joining the connect conversation:
    -- - MemberJoin event to self and other (via galley)
    Named
      "update-connection"
      ( Summary "Update a connection to another user"
          :> MakesFederatedCall 'Brig "get-users-by-ids"
          :> MakesFederatedCall 'Brig "send-connection-action"
          :> CanThrow 'MissingLegalholdConsentOldClients
          :> CanThrow 'MissingLegalholdConsent
          :> CanThrow 'InvalidUser
          :> CanThrow 'ConnectionLimitReached
          :> CanThrow 'NotConnected
          :> CanThrow 'InvalidTransition
          :> CanThrow 'NoIdentity
          :> ZUser
          :> ZConn
          :> "connections"
          :> QualifiedCaptureUserId "uid"
          :> ReqBody '[JSON] ConnectionUpdate
          :> MultiVerb
               'PUT
               '[JSON]
               ConnectionUpdateResponses
               (UpdateResult UserConnection)
      )
    :<|> Named
           "search-contacts"
           ( Summary "Search for users"
               :> MakesFederatedCall 'Brig "get-users-by-ids"
               :> MakesFederatedCall 'Brig "search-users"
               :> ZUser
               :> "search"
               :> "contacts"
               :> QueryParam' '[Required, Strict, Description "Search query"] "q" Text
               :> QueryParam' '[Optional, Strict, Description "Searched domain. Note: This is optional only for backwards compatibility, future versions will mandate this."] "domain" Domain
               :> QueryParam' '[Optional, Strict, Description "Number of results to return (min: 1, max: 500, default 15)"] "size" (Range 1 500 Int32)
               :> Get '[Servant.JSON] (SearchResult Contact)
           )

-- Properties API -----------------------------------------------------

type PropertiesAPI =
  LiftNamed
    ( ZUser
        :> "properties"
        :> ( Named
               "set-property"
               -- This endpoint can lead to the following events being sent:
               -- - PropertySet event to self
               ( Summary "Set a user property"
                   :> ZConn
                   :> Capture "key" PropertyKey
                   :> ReqBody '[JSON] RawPropertyValue
                   :> MultiVerb1 'PUT '[JSON] (RespondEmpty 200 "Property set")
               )
               :<|>
               -- This endpoint can lead to the following events being sent:
               -- - PropertyDeleted event to self
               Named
                 "delete-property"
                 ( Summary "Delete a property"
                     :> ZConn
                     :> Capture "key" PropertyKey
                     :> MultiVerb1 'DELETE '[JSON] (RespondEmpty 200 "Property deleted")
                 )
               :<|>
               -- This endpoint can lead to the following events being sent:
               -- - PropertiesCleared event to self
               Named
                 "clear-properties"
                 ( Summary "Clear all properties"
                     :> ZConn
                     :> MultiVerb1 'DELETE '[JSON] (RespondEmpty 200 "Properties cleared")
                 )
               :<|> Named
                      "get-property"
                      ( Summary "Get a property value"
                          :> Capture "key" PropertyKey
                          :> MultiVerb
                               'GET
                               '[JSON]
                               '[ EmptyErrorForLegacyReasons 404 "Property not found",
                                  Respond 200 "The property value" RawPropertyValue
                                ]
                               (Maybe RawPropertyValue)
                      )
               :<|> Named
                      "list-property-keys"
                      ( Summary "List all property keys"
                          :> MultiVerb1 'GET '[JSON] (Respond 200 "List of property keys" [PropertyKey])
                      )
           )
    )
    :<|> Named
           "list-properties"
           ( Summary "List all properties with key and value"
               :> ZUser
               :> "properties-values"
               :> Get '[JSON] PropertyKeysAndValues
           )

-- MLS API ---------------------------------------------------------------------

type CipherSuiteParam =
  QueryParam'
    [ Optional,
      Strict,
      Description "Ciphersuite in hex format (e.g. 0xf031) - default is 0x0001"
    ]
    "ciphersuite"
    CipherSuite

type MultipleCipherSuitesParam =
  QueryParam'
    [ Optional,
      Strict,
      Description "Comma-separated list of ciphersuites in hex format (e.g. 0xf031) - default is 0x0001"
    ]
    "ciphersuites"
    (CommaSeparatedList CipherSuite)

type MLSKeyPackageAPI =
  "key-packages"
    :> ( Named
           "mls-key-packages-upload"
           ( "self"
               :> Summary "Upload a fresh batch of key packages"
               :> From 'V5
               :> Description "The request body should be a json object containing a list of base64-encoded key packages."
               :> ZLocalUser
               :> CanThrow 'MLSProtocolError
               :> CanThrow 'MLSIdentityMismatch
               :> CaptureClientId "client"
               :> ReqBody '[JSON] KeyPackageUpload
               :> MultiVerb 'POST '[JSON, MLS] '[RespondEmpty 201 "Key packages uploaded"] ()
           )
           :<|> Named
                  "mls-key-packages-replace"
                  ( "self"
                      :> Summary "Upload a fresh batch of key packages and replace the old ones"
                      :> From 'V5
                      :> Description "The request body should be a json object containing a list of base64-encoded key packages. Use this sparingly."
                      :> ZLocalUser
                      :> CanThrow 'MLSProtocolError
                      :> CanThrow 'MLSIdentityMismatch
                      :> CaptureClientId "client"
                      :> MultipleCipherSuitesParam
                      :> ReqBody '[JSON] KeyPackageUpload
                      :> MultiVerb 'PUT '[JSON, MLS] '[RespondEmpty 201 "Key packages replaced"] ()
                  )
           :<|> Named
                  "mls-key-packages-claim"
                  ( "claim"
                      :> Summary "Claim one key package for each client of the given user"
                      :> From 'V5
                      :> Description "Only key packages for the specified ciphersuite are claimed. For backwards compatibility, the `ciphersuite` parameter is optional, defaulting to ciphersuite 0x0001 when omitted."
                      :> ZLocalUser
                      :> ZOptClient
                      :> QualifiedCaptureUserId "user"
                      :> CipherSuiteParam
                      :> MultiVerb1 'POST '[JSON] (Respond 200 "Claimed key packages" KeyPackageBundle)
                  )
           :<|> Named
                  "mls-key-packages-count"
                  ( "self"
                      :> Summary "Return the number of unclaimed key packages for a given ciphersuite and client"
                      :> From 'V5
                      :> ZLocalUser
                      :> CaptureClientId "client"
                      :> "count"
                      :> CipherSuiteParam
                      :> MultiVerb1 'GET '[JSON] (Respond 200 "Number of key packages" KeyPackageCount)
                  )
           :<|> Named
                  "mls-key-packages-delete"
                  ( "self"
                      :> From 'V5
                      :> ZLocalUser
                      :> CaptureClientId "client"
                      :> Summary "Delete all key packages for a given ciphersuite and client"
                      :> CipherSuiteParam
                      :> ReqBody '[JSON] DeleteKeyPackages
                      :> MultiVerb1 'DELETE '[JSON] (RespondEmpty 201 "OK")
                  )
       )

type MLSAPI = LiftNamed ("mls" :> MLSKeyPackageAPI)

-- Search API -----------------------------------------------------

type SearchAPI =
  Named
    "browse-team"
    ( Summary "Browse team for members (requires add-user permission)"
        :> ZUser
        :> "teams"
        :> Capture "tid" TeamId
        :> "search"
        :> QueryParam'
             [ Optional,
               Strict,
               Description "Search expression"
             ]
             "q"
             Text
        :> QueryParam'
             [ Optional,
               Strict,
               Description "Role filter, eg. `member,partner`.  Empty list means do not filter."
             ]
             "frole"
             RoleFilter
        :> QueryParam'
             [ Optional,
               Strict,
               Description "Can be one of name, handle, email, saml_idp, managed_by, role, created_at."
             ]
             "sortby"
             TeamUserSearchSortBy
        :> QueryParam'
             [ Optional,
               Strict,
               Description "Can be one of asc, desc."
             ]
             "sortorder"
             TeamUserSearchSortOrder
        :> QueryParam'
             [ Optional,
               Strict,
               Description "Number of results to return (min: 1, max: 500, default: 15)"
             ]
             "size"
             (Range 1 500 Int32)
        :> QueryParam'
             [ Optional,
               Strict,
               Description
                 "Optional, when not specified, the first page will be returned. \
                 \Every returned page contains a `paging_state`, this should be supplied to retrieve the next page."
             ]
             "pagingState"
             PagingState
        :> MultiVerb
             'GET
             '[JSON]
             '[Respond 200 "Search results" (SearchResult TeamContact)]
             (SearchResult TeamContact)
    )

type AuthAPI =
  Named
    "access"
    ( "access"
        :> Summary "Obtain an access tokens for a cookie"
        :> Description
             "You can provide only a cookie or a cookie and token.\
             \ Every other combination is invalid.\
             \ Access tokens can be given as query parameter or authorisation\
             \ header, with the latter being preferred."
        :> MakesFederatedCall 'Brig "send-connection-action"
        :> QueryParam "client_id" ClientId
        :> Cookies '["zuid" ::: SomeUserToken]
        :> Bearer SomeAccessToken
        :> CanThrow 'BadCredentials
        :> MultiVerb1 'POST '[JSON] TokenResponse
    )
    :<|> Named
           "send-login-code"
           ( "login"
               :> "send"
               :> Summary "Send a login code to a verified phone number"
               :> Description
                    "This operation generates and sends a login code via sms for phone login.\
                    \ A login code can be used only once and times out after\
                    \ 10 minutes. Only one login code may be pending at a time.\
                    \ For 2nd factor authentication login with email and password, use the\
                    \ `/verification-code/send` endpoint."
               :> ReqBody '[JSON] SendLoginCode
               :> CanThrow 'InvalidPhone
               :> CanThrow 'PasswordExists
               :> MultiVerb1
                    'POST
                    '[JSON]
                    (Respond 200 "OK" LoginCodeTimeout)
           )
    :<|> Named
           "login"
           ( "login"
               :> Summary "Authenticate a user to obtain a cookie and first access token"
               :> Description "Logins are throttled at the server's discretion"
               :> MakesFederatedCall 'Brig "send-connection-action"
               :> ReqBody '[JSON] Login
               :> QueryParam'
                    [ Optional,
                      Strict,
                      Description "Request a persistent cookie instead of a session cookie"
                    ]
                    "persist"
                    Bool
               :> CanThrow 'BadCredentials
               :> CanThrow 'AccountSuspended
               :> CanThrow 'AccountPending
               :> CanThrow 'CodeAuthenticationFailed
               :> CanThrow 'CodeAuthenticationRequired
               :> MultiVerb1 'POST '[JSON] TokenResponse
           )
    :<|> Named
           "logout"
           ( "access"
               :> "logout"
               :> Summary "Log out in order to remove a cookie from the server"
               :> Description
                    "Calling this endpoint will effectively revoke the given cookie\
                    \ and subsequent calls to /access with the same cookie will\
                    \ result in a 403."
               :> Cookies '["zuid" ::: SomeUserToken]
               :> Bearer SomeAccessToken
               :> CanThrow 'BadCredentials
               :> MultiVerb1 'POST '[JSON] (RespondEmpty 200 "Logout")
           )
    :<|> Named
           "change-self-email"
           ( "access"
               :> "self"
               :> "email"
               :> Summary "Change your email address"
               :> Cookies '["zuid" ::: SomeUserToken]
               :> Bearer SomeAccessToken
               :> ReqBody '[JSON] EmailUpdate
               :> CanThrow 'InvalidEmail
               :> CanThrow 'UserKeyExists
               :> CanThrow 'BlacklistedEmail
               :> CanThrow 'BlacklistedPhone
               :> CanThrow 'BadCredentials
               :> MultiVerb
                    'PUT
                    '[JSON]
                    '[ Respond 202 "Update accepted and pending activation of the new email" (),
                       Respond 204 "No update, current and new email address are the same" ()
                     ]
                    ChangeEmailResponse
           )
    :<|> Named
           "list-cookies"
           ( "cookies"
               :> Summary "Retrieve the list of cookies currently stored for the user"
               :> ZLocalUser
               :> QueryParam'
                    [Optional, Strict, Description "Filter by label (comma-separated list)"]
                    "labels"
                    (CommaSeparatedList CookieLabel)
               :> MultiVerb1 'GET '[JSON] (Respond 200 "List of cookies" CookieList)
           )
    :<|> Named
           "remove-cookies"
           ( "cookies"
               :> "remove"
               :> Summary "Revoke stored cookies"
               :> ZLocalUser
               :> CanThrow 'BadCredentials
               :> ReqBody '[JSON] RemoveCookies
               :> MultiVerb1 'POST '[JSON] (RespondEmpty 200 "Cookies revoked")
           )

-------------------------------------------------------------------------------
-- Calling API

type CallingAPI =
  -- Deprecated endpoint, but still used by old clients.
  -- See https://github.com/zinfra/backend-issues/issues/1616 for context
  Named
    "get-calls-config"
    ( Summary
        "Retrieve TURN server addresses and credentials for \
        \ IP addresses, scheme `turn` and transport `udp` only (deprecated)"
        :> Deprecated
        :> ZUser
        :> ZConn
        :> "calls"
        :> "config"
        :> Get '[JSON] RTCConfiguration
    )
    :<|> Named
           "get-calls-config-v2"
           ( Summary
               "Retrieve all TURN server addresses and credentials. \
               \Clients are expected to do a DNS lookup to resolve \
               \the IP addresses of the given hostnames "
               :> ZUser
               :> ZConn
               :> "calls"
               :> "config"
               :> "v2"
               :> QueryParam' '[Optional, Strict, Description "Limit resulting list. Allowed values [1..10]"] "limit" (Range 1 10 Int)
               :> Get '[JSON] RTCConfiguration
           )

-- Teams API -----------------------------------------------------

type TeamsAPI =
  Named
    "send-team-invitation"
    ( Summary "Create and send a new team invitation."
        :> Description
             "Invitations are sent by email. The maximum allowed number of \
             \pending team invitations is equal to the team size."
        :> CanThrow 'NoEmail
        :> CanThrow 'NoIdentity
        :> CanThrow 'InvalidEmail
        :> CanThrow 'BlacklistedEmail
        :> CanThrow 'TooManyTeamInvitations
        :> CanThrow 'InsufficientTeamPermissions
        :> ZUser
        :> "teams"
        :> Capture "tid" TeamId
        :> "invitations"
        :> ReqBody '[JSON] InvitationRequest
        :> MultiVerb1
             'POST
             '[JSON]
             ( WithHeaders
                 '[Header "Location" InvitationLocation]
                 (Invitation, InvitationLocation)
                 (Respond 201 "Invitation was created and sent." Invitation)
             )
    )
    :<|> Named
           "get-team-invitations"
           ( Summary "List the sent team invitations"
               :> CanThrow 'InsufficientTeamPermissions
               :> ZUser
               :> "teams"
               :> Capture "tid" TeamId
               :> "invitations"
               :> QueryParam' '[Optional, Strict, Description "Invitation id to start from (ascending)."] "start" InvitationId
               :> QueryParam' '[Optional, Strict, Description "Number of results to return (default 100, max 500)."] "size" (Range 1 500 Int32)
               :> MultiVerb1
                    'GET
                    '[JSON]
                    (Respond 200 "List of sent invitations" InvitationList)
           )
    :<|> Named
           "get-team-invitation"
           ( Summary "Get a pending team invitation by ID."
               :> CanThrow 'InsufficientTeamPermissions
               :> ZUser
               :> "teams"
               :> Capture "tid" TeamId
               :> "invitations"
               :> Capture "iid" InvitationId
               :> MultiVerb
                    'GET
                    '[JSON]
                    '[ ErrorResponse 'NotificationNotFound,
                       Respond 200 "Invitation" Invitation
                     ]
                    (Maybe Invitation)
           )
    :<|> Named
           "delete-team-invitation"
           ( Summary "Delete a pending team invitation by ID."
               :> CanThrow 'InsufficientTeamPermissions
               :> ZUser
               :> "teams"
               :> Capture "tid" TeamId
               :> "invitations"
               :> Capture "iid" InvitationId
               :> MultiVerb1 'DELETE '[JSON] (RespondEmpty 200 "Invitation deleted")
           )
    :<|> Named
           "get-team-invitation-info"
           ( Summary "Get invitation info given a code."
               :> CanThrow 'InvalidInvitationCode
               :> "teams"
               :> "invitations"
               :> "info"
               :> QueryParam' '[Required, Strict, Description "Invitation code"] "code" InvitationCode
               :> MultiVerb1
                    'GET
                    '[JSON]
                    (Respond 200 "Invitation info" Invitation)
           )
    -- FUTUREWORK: Add another endpoint to allow resending of invitation codes
    :<|> Named
           "head-team-invitations"
           ( Summary "Check if there is an invitation pending given an email address."
               :> "teams"
               :> "invitations"
               :> "by-email"
               :> QueryParam' '[Required, Strict, Description "Email address"] "email" Email
               :> MultiVerb
                    'HEAD
                    '[JSON]
                    HeadInvitationsResponses
                    HeadInvitationByEmailResult
           )
    :<|> Named
           "get-team-size"
           ( Summary "Get the number of team members as an integer"
               :> Description
                    "Can be out of sync by roughly the `refresh_interval` \
                    \of the ES index."
               :> CanThrow 'InvalidInvitationCode
               :> ZUser
               :> "teams"
               :> Capture "tid" TeamId
               :> "size"
               :> MultiVerb1
                    'GET
                    '[JSON]
                    (Respond 200 "Number of team members" TeamSize)
           )

type SystemSettingsAPI =
  Named
    "get-system-settings-unauthorized"
    ( Summary "Returns a curated set of system configuration settings."
        :> From 'V3
        :> "system"
        :> "settings"
        :> "unauthorized"
        :> Get '[JSON] SystemSettingsPublic
    )
    :<|> Named
           "get-system-settings"
           ( Summary "Returns a curated set of system configuration settings for authorized users."
               :> From 'V4
               :> ZUser
               :> "system"
               :> "settings"
               :> Get '[JSON] SystemSettings
           )

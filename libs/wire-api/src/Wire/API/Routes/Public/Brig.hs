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

import Data.Code (Timeout)
import Data.CommaSeparatedList (CommaSeparatedList)
import Data.Domain
import Data.Handle
import Data.Id as Id
import Data.Misc (IpAddr)
import Data.Qualified (Qualified (..))
import Data.Range
import Data.SOP (I (..), NS (..))
import Data.Swagger hiding (Contact, Header)
import Imports hiding (head)
import Servant (JSON)
import Servant hiding (Handler, JSON, addHeader, respond)
import Servant.Swagger (HasSwagger (toSwagger))
import Servant.Swagger.Internal.Orphans ()
import Wire.API.Connection
import Wire.API.ErrorDescription
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Servant
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.Routes.Public
import Wire.API.Routes.Public.Util
import Wire.API.Routes.QualifiedCapture
import Wire.API.User
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.User.Handle
import Wire.API.User.Search (Contact, SearchResult)
import Wire.API.UserMap

type MaxUsersForListClientsBulk = 500

type GetUserVerb =
  MultiVerb
    'GET
    '[JSON]
    '[ UserNotFound,
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
  fromUnion (S (S x)) = case x of

type ConnectionUpdateResponses = UpdateResponses "Connection unchanged" "Connection updated" UserConnection

type UserAPI =
  -- See Note [ephemeral user sideeffect]
  Named
    "get-user-unqualified"
    ( Summary "Get a user by UserId (deprecated)"
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
               :> ZUser
               :> "users"
               :> "handles"
               :> Capture' '[Description "The user handle"] "handle" Handle
               :> MultiVerb
                    'GET
                    '[JSON]
                    '[ HandleNotFound,
                       Respond 200 "User found" UserHandleInfo
                     ]
                    (Maybe UserHandleInfo)
           )
    :<|> Named
           "get-user-by-handle-qualified"
           ( Summary "(deprecated, use /search/contacts) Get information on a user handle"
               :> ZUser
               :> "users"
               :> "by-handle"
               :> QualifiedCapture' '[Description "The user handle"] "handle" Handle
               :> MultiVerb
                    'GET
                    '[JSON]
                    '[ HandleNotFound,
                       Respond 200 "User found" UserProfile
                     ]
                    (Maybe UserProfile)
           )
    :<|>
    -- See Note [ephemeral user sideeffect]
    Named
      "list-users-by-unqualified-ids-or-handles"
      ( Summary "List users (deprecated)"
          :> Description "The 'ids' and 'handles' parameters are mutually exclusive."
          :> ZUser
          :> "users"
          :> QueryParam' [Optional, Strict, Description "User IDs of users to fetch"] "ids" (CommaSeparatedList UserId)
          :> QueryParam' [Optional, Strict, Description "Handles of users to fetch, min 1 and max 4 (the check for handles is rather expensive)"] "handles" (Range 1 4 (CommaSeparatedList Handle))
          :> Get '[JSON] [UserProfile]
      )
    :<|>
    -- See Note [ephemeral user sideeffect]
    Named
      "list-users-by-ids-or-handles"
      ( Summary "List users"
          :> Description "The 'qualified_ids' and 'qualified_handles' parameters are mutually exclusive."
          :> ZUser
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

type SelfAPI =
  Named
    "get-self"
    ( Summary "Get your own profile"
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
          :> CanThrow InvalidUser
          :> CanThrow InvalidCode
          :> CanThrow BadCredentials
          :> CanThrow MissingAuth
          :> CanThrow DeleteCodePending
          :> CanThrow OwnerDeletingSelf
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
               :> ZUser
               :> ZConn
               :> "self"
               :> "handle"
               :> ReqBody '[JSON] HandleUpdate
               :> MultiVerb 'PUT '[JSON] ChangeHandleResponses (Maybe ChangeHandleError)
           )

type PrekeyAPI =
  Named
    "get-users-prekeys-client-unqualified"
    ( Summary "(deprecated) Get a prekey for a specific client of a user."
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
               :> ZUser
               :> "users"
               :> CaptureUserId "uid"
               :> "prekeys"
               :> Get '[JSON] PrekeyBundle
           )
    :<|> Named
           "get-users-prekey-bundle-qualified"
           ( Summary "Get a prekey for each client of a user."
               :> ZUser
               :> "users"
               :> QualifiedCaptureUserId "uid"
               :> "prekeys"
               :> Get '[JSON] PrekeyBundle
           )
    :<|> Named
           "get-multi-user-prekey-bundle-unqualified"
           ( Summary
               "(deprecated)  Given a map of user IDs to client IDs return a \
               \prekey for each one. You can't request information for more users than \
               \maximum conversation size."
               :> ZUser
               :> "users"
               :> "prekeys"
               :> ReqBody '[JSON] UserClients
               :> Post '[JSON] UserClientPrekeyMap
           )
    :<|> Named
           "get-multi-user-prekey-bundle-qualified"
           ( Summary
               "Given a map of domain to (map of user IDs to client IDs) return a \
               \prekey for each one. You can't request information for more users than \
               \maximum conversation size."
               :> ZUser
               :> "users"
               :> "list-prekeys"
               :> ReqBody '[JSON] QualifiedUserClients
               :> Post '[JSON] QualifiedUserClientPrekeyMap
           )

type UserClientAPI =
  -- User Client API ----------------------------------------------------

  -- This endpoint can lead to the following events being sent:
  -- - ClientAdded event to self
  -- - ClientRemoved event to self, if removing old clients due to max number
  Named
    "add-client"
    ( Summary "Register a new client"
        :> CanThrow TooManyClients
        :> CanThrow MissingAuth
        :> CanThrow MalformedPrekeys
        :> ZUser
        :> ZConn
        :> "clients"
        :> Header "X-Forwarded-For" IpAddr
        :> ReqBody '[JSON] NewClient
        :> Verb 'POST 201 '[JSON] NewClientResponse
    )
    :<|> Named
           "update-client"
           ( Summary "Update a registered client"
               :> CanThrow MalformedPrekeys
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

type ClientAPI =
  Named
    "get-user-clients-unqualified"
    ( Summary "Get all of a user's clients (deprecated)."
        :> "users"
        :> CaptureUserId "uid"
        :> "clients"
        :> Get '[JSON] [PubClient]
    )
    :<|> Named
           "get-user-clients-qualified"
           ( Summary "Get all of a user's clients."
               :> "users"
               :> QualifiedCaptureUserId "uid"
               :> "clients"
               :> Get '[JSON] [PubClient]
           )
    :<|> Named
           "get-user-client-unqualified"
           ( Summary "Get a specific client of a user (deprecated)."
               :> "users"
               :> CaptureUserId "uid"
               :> "clients"
               :> CaptureClientId "client"
               :> Get '[JSON] PubClient
           )
    :<|> Named
           "get-user-client-qualified"
           ( Summary "Get a specific client of a user."
               :> "users"
               :> QualifiedCaptureUserId "uid"
               :> "clients"
               :> CaptureClientId "client"
               :> Get '[JSON] PubClient
           )
    :<|> Named
           "list-clients-bulk"
           ( Summary "List all clients for a set of user ids (deprecated, use /users/list-clients/v2)"
               :> ZUser
               :> "users"
               :> "list-clients"
               :> ReqBody '[JSON] (Range 1 MaxUsersForListClientsBulk [Qualified UserId])
               :> Post '[JSON] (QualifiedUserMap (Set PubClient))
           )
    :<|> Named
           "list-clients-bulk-v2"
           ( Summary "List all clients for a set of user ids"
               :> ZUser
               :> "users"
               :> "list-clients"
               :> "v2"
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
    ( Summary "Create a connection to another user (deprecated)"
        :> CanThrow MissingLegalholdConsent
        :> CanThrow InvalidUser
        :> CanThrow ConnectionLimitReached
        :> CanThrow NoIdentity
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
               :> CanThrow MissingLegalholdConsent
               :> CanThrow InvalidUser
               :> CanThrow ConnectionLimitReached
               :> CanThrow NoIdentity
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
           ( Summary "List the local connections to other users (deprecated)"
               :> ZUser
               :> "connections"
               :> QueryParam' '[Optional, Strict, Description "User ID to start from when paginating"] "start" UserId
               :> QueryParam' '[Optional, Strict, Description "Number of results to return (default 100, max 500)"] "size" (Range 1 500 Int32)
               :> Get '[JSON] UserConnectionList
           )
    :<|> Named
           "list-connections"
           ( Summary "List the connections to other users, including remote users"
               :> ZUser
               :> "list-connections"
               :> ReqBody '[JSON] ListConnectionsRequestPaginated
               :> Post '[JSON] ConnectionsPage
           )
    :<|> Named
           "get-connection-unqualified"
           ( Summary "Get an existing connection to another user (deprecated)"
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
      ( Summary "Update a connection to another user (deprecated)"
          :> CanThrow MissingLegalholdConsent
          :> CanThrow InvalidUser
          :> CanThrow ConnectionLimitReached
          :> CanThrow NotConnected
          :> CanThrow InvalidTransition
          :> CanThrow NoIdentity
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
      ( Summary "Update a connection to another user (deprecatd)"
          :> CanThrow MissingLegalholdConsent
          :> CanThrow InvalidUser
          :> CanThrow ConnectionLimitReached
          :> CanThrow NotConnected
          :> CanThrow InvalidTransition
          :> CanThrow NoIdentity
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
               :> ZUser
               :> "search"
               :> "contacts"
               :> QueryParam' '[Required, Strict, Description "Search query"] "q" Text
               :> QueryParam' '[Optional, Strict, Description "Searched domain. Note: This is optional only for backwards compatibility, future versions will mandate this."] "domain" Domain
               :> QueryParam' '[Optional, Strict, Description "Number of results to return (min: 1, max: 500, default 15)"] "size" (Range 1 500 Int32)
               :> Get '[Servant.JSON] (SearchResult Contact)
           )

type MLSKeyPackageAPI =
  "key-packages"
    :> ( Named
           "mls-key-packages-upload"
           ( "self"
               :> Summary "Upload a fresh batch of key packages"
               :> Description "The request body should be a json object containing a list of base64-encoded key packages."
               :> CanThrow MLSProtocolError
               :> CanThrow MLSIdentityMismatch
               :> CaptureClientId "client"
               :> ReqBody '[JSON] KeyPackageUpload
               :> MultiVerb 'POST '[JSON, MLS] '[RespondEmpty 200 "Key packages uploaded"] ()
           )
           :<|> ( Named
                    "mls-key-packages-claim"
                    ( "claim"
                        :> Summary "Claim one key package for each client of the given user"
                        :> QualifiedCaptureUserId "user"
                        :> MultiVerb1 'POST '[JSON] (Respond 200 "Claimed key packages" KeyPackageBundle)
                    )
                )
           :<|> ( Named
                    "mls-key-packages-count"
                    ( "count"
                        :> Summary "Return the number of unused key packages for the given client"
                        :> CaptureClientId "client"
                        :> MultiVerb1 'GET '[JSON] (Respond 200 "Number of key packages" KeyPackageCount)
                    )
                )
       )

type MLSAPI = LiftNamed (ZLocalUser :> "mls" :> MLSKeyPackageAPI)

type BrigAPI =
  UserAPI
    :<|> SelfAPI
    :<|> ClientAPI
    :<|> PrekeyAPI
    :<|> UserClientAPI
    :<|> ConnectionAPI
    :<|> MLSAPI

brigSwagger :: Swagger
brigSwagger = toSwagger (Proxy @BrigAPI)

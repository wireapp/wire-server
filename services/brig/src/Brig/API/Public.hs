{-# LANGUAGE RecordWildCards #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Brig.API.Public
  ( sitemap,
    apiDocs,
  )
where

import qualified Brig.API.Client as API
import qualified Brig.API.Connection as API
import Brig.API.Error
import Brig.API.Handler
import Brig.API.IdMapping (resolveOpaqueUserId)
import qualified Brig.API.Properties as API
import Brig.API.Types
import qualified Brig.API.User as API
import Brig.App
import qualified Brig.Data.User as Data
import Brig.Options hiding (internalEvents, sesQueue)
import qualified Brig.Provider.API as Provider
import qualified Brig.TURN.API as TURN
import qualified Brig.Team.API as Team
import qualified Brig.Team.Email as Team
import Brig.Types.Intra (AccountStatus (Ephemeral), UserAccount (UserAccount, accountUser))
import qualified Brig.User.API.Auth as Auth
import qualified Brig.User.API.Search as Search
import qualified Brig.User.Auth.Cookie as Auth
import Brig.User.Email
import Brig.User.Phone
import Control.Error hiding (bool)
import Control.Lens (view, (^.))
import Control.Monad.Catch (throwM)
import Data.Aeson hiding (json)
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as Lazy
import Data.Domain (mkDomain)
import Data.Handle (Handle, parseHandle)
import Data.Id as Id
import Data.IdMapping (MappedOrLocalId (Local))
import qualified Data.Map.Strict as Map
import Data.Misc (IpAddr (..), (<$$>))
import Data.Qualified (OptionallyQualified, eitherQualifiedOrNot)
import Data.Range
import qualified Data.Swagger.Build.Api as Doc
import qualified Data.Text as Text
import qualified Data.Text.Ascii as Ascii
import Data.Text.Encoding (decodeLatin1)
import Data.Text.Lazy (pack)
import qualified Data.ZAuth.Token as ZAuth
import Imports hiding (head)
import Network.HTTP.Types.Status
import Network.Wai (Response, lazyRequestBody)
import Network.Wai.Predicate hiding (result, setStatus)
import Network.Wai.Routing
import Network.Wai.Utilities as Utilities
import Network.Wai.Utilities.Response (json)
import Network.Wai.Utilities.Swagger (document, mkSwaggerApi)
import qualified Network.Wai.Utilities.Swagger as Doc
import Network.Wai.Utilities.ZAuth (zauthConnId, zauthUserId)
import qualified Wire.API.Connection as Public
import qualified Wire.API.Properties as Public
import qualified Wire.API.Swagger as Public.Swagger (models)
import qualified Wire.API.Team as Public
import qualified Wire.API.User as Public
import qualified Wire.API.User.Activation as Public
import qualified Wire.API.User.Auth as Public
import qualified Wire.API.User.Client as Public
import qualified Wire.API.User.Client.Prekey as Public
import qualified Wire.API.User.Handle as Public
import qualified Wire.API.User.Password as Public
import qualified Wire.API.User.RichInfo as Public

---------------------------------------------------------------------------
-- Sitemap

sitemap :: Opts -> Routes Doc.ApiBuilder Handler ()
sitemap o = do
  -- User API -----------------------------------------------------------

  -- If the user is ephemeral and expired, it will be removed, see 'Brig.API.User.userGC'.
  -- This leads to the following events being sent:
  -- - UserDeleted event to contacts of the user
  -- - MemberLeave event to members for all conversations the user was in (via galley)
  head "/users/:uid" (continue checkUserExistsH) $
    zauthUserId
      .&. capture "uid"
  document "HEAD" "userExists" $ do
    Doc.summary "Check if a user ID exists"
    Doc.parameter Doc.Path "uid" Doc.bytes' $
      Doc.description "User ID"
    Doc.response 200 "User exists" Doc.end
    Doc.errorResponse userNotFound

  -- If the user is ephemeral and expired, it will be removed, see 'Brig.API.User.userGC'.
  -- This leads to the following events being sent:
  -- - UserDeleted event to contacts of the user
  -- - MemberLeave event to members for all conversations the user was in (via galley)
  get "/users/:uid" (continue getUserH) $
    accept "application" "json"
      .&. zauthUserId
      .&. capture "uid"
  document "GET" "user" $ do
    Doc.summary "Get a user by ID"
    Doc.parameter Doc.Path "uid" Doc.bytes' $
      Doc.description "User ID"
    Doc.returns (Doc.ref Public.modelUser)
    Doc.response 200 "User" Doc.end
    Doc.errorResponse userNotFound

  -- User Handle API ----------------------------------------------------

  post "/users/handles" (continue checkHandlesH) $
    accept "application" "json"
      .&. zauthUserId
      .&. jsonRequest @Public.CheckHandles
  document "POST" "checkUserHandles" $ do
    Doc.summary "Check availability of user handles"
    Doc.body (Doc.ref Public.modelCheckHandles) $
      Doc.description "JSON body"
    Doc.returns (Doc.array Doc.string')
    Doc.response 200 "List of free handles" Doc.end

  head "/users/handles/:handle" (continue checkHandleH) $
    zauthUserId
      .&. capture "handle"
  document "HEAD" "checkUserHandle" $ do
    Doc.summary "Check whether a user handle can be taken"
    Doc.parameter Doc.Path "handle" Doc.bytes' $
      Doc.description "Handle to check"
    Doc.response 200 "Handle is taken" Doc.end
    Doc.errorResponse invalidHandle
    Doc.errorResponse handleNotFound

  get "/users/handles/:handle" (continue getHandleInfoH) $
    accept "application" "json"
      .&. zauthUserId
      .&. capture "handle"
  document "GET" "getUserHandleInfo" $ do
    Doc.summary "Get information on a user handle"
    Doc.parameter Doc.Path "handle" Doc.bytes' $
      Doc.description "The user handle"
    Doc.returns (Doc.ref Public.modelUserHandleInfo)
    Doc.response 200 "Handle info" Doc.end
    Doc.errorResponse handleNotFound

  -- end User Handle API

  -- If the user is ephemeral and expired, it will be removed, see 'Brig.API.User.userGC'.
  -- This leads to the following events being sent:
  -- - UserDeleted event to contacts of the user
  -- - MemberLeave event to members for all conversations the user was in (via galley)
  get "/users" (continue listUsersH) $
    accept "application" "json"
      .&. zauthUserId
      .&. (param "ids" ||| param "handles")
  document "GET" "users" $ do
    Doc.summary "List users"
    Doc.notes "The 'ids' and 'handles' parameters are mutually exclusive."
    Doc.parameter Doc.Query "ids" Doc.string' $ do
      Doc.description "User IDs of users to fetch"
      Doc.optional
    Doc.parameter Doc.Query "handles" Doc.string' $ do
      Doc.description "Handles of users to fetch, min 1 and max 4 (the check for handles is rather expensive)"
      Doc.optional
    Doc.returns (Doc.array (Doc.ref Public.modelUser))
    Doc.response 200 "List of users" Doc.end

  -- User Prekey API ----------------------------------------------------

  post "/users/prekeys" (continue getMultiPrekeyBundlesH) $
    jsonRequest @Public.UserClients
      .&. accept "application" "json"
  document "POST" "getMultiPrekeyBundles" $ do
    Doc.summary
      "Given a map of user IDs to client IDs return a \
      \prekey for each one. You can't request information for more users than \
      \maximum conversation size."
    Doc.notes
      "Prekeys of all clients of a multiple users. \
      \The result is a map of maps, i.e. { UserId : { ClientId : Maybe Prekey } }"
    Doc.body (Doc.ref Public.modelUserClients) $
      Doc.description "JSON body"
    Doc.response 200 "Prekey Bundles" Doc.end
    Doc.errorResponse tooManyClients

  get "/users/:uid/prekeys" (continue getPrekeyBundleH) $
    capture "uid"
      .&. accept "application" "json"
  document "GET" "getPrekeyBundle" $ do
    Doc.summary "Get a prekey for each client of a user."
    Doc.parameter Doc.Path "uid" Doc.bytes' $
      Doc.description "User ID"
    Doc.returns (Doc.ref Public.modelPrekeyBundle)
    Doc.response 200 "Prekey Bundle" Doc.end

  get "/users/:uid/prekeys/:client" (continue getPrekeyH) $
    capture "uid"
      .&. capture "client"
      .&. accept "application" "json"
  document "GET" "getPrekey" $ do
    Doc.summary "Get a prekey for a specific client of a user."
    Doc.parameter Doc.Path "uid" Doc.bytes' $
      Doc.description "User ID"
    Doc.parameter Doc.Path "client" Doc.bytes' $
      Doc.description "Client ID"
    Doc.returns (Doc.ref Public.modelClientPrekey)
    Doc.response 200 "Client Prekey" Doc.end

  -- User Client API ----------------------------------------------------

  get "/users/:uid/clients" (continue getUserClientsH) $
    capture "uid"
      .&. accept "application" "json"
  document "GET" "getUserClients" $ do
    Doc.summary "Get all of a user's clients."
    Doc.parameter Doc.Path "uid" Doc.bytes' $
      Doc.description "User ID"
    Doc.returns (Doc.array (Doc.ref Public.modelPubClient))
    Doc.response 200 "List of clients" Doc.end

  get "/users/:uid/clients/:client" (continue getUserClientH) $
    capture "uid"
      .&. capture "client"
      .&. accept "application" "json"
  document "GET" "getUserClient" $ do
    Doc.summary "Get a specific client of a user."
    Doc.parameter Doc.Path "uid" Doc.bytes' $
      Doc.description "User ID"
    Doc.parameter Doc.Path "client" Doc.bytes' $
      Doc.description "Client ID"
    Doc.returns (Doc.ref Public.modelPubClient)
    Doc.response 200 "Client" Doc.end

  -- end User Client API

  get "/users/:uid/rich-info" (continue getRichInfoH) $
    zauthUserId
      .&. capture "uid"
      .&. accept "application" "json"
  document "GET" "getRichInfo" $ do
    Doc.summary "Get user's rich info"
    Doc.parameter Doc.Path "uid" Doc.bytes' $
      Doc.description "User ID"
    Doc.returns (Doc.ref Public.modelRichInfo)
    Doc.response 200 "RichInfo" Doc.end
    Doc.errorResponse insufficientTeamPermissions

  -- User Self API ------------------------------------------------------

  get "/self" (continue getSelfH) $
    accept "application" "json"
      .&. zauthUserId
  document "GET" "self" $ do
    Doc.summary "Get your profile"
    Doc.returns (Doc.ref Public.modelSelf)
    Doc.response 200 "Self profile" Doc.end

  -- This endpoint can lead to the following events being sent:
  -- - UserUpdated event to contacts of self
  put "/self" (continue updateUserH) $
    zauthUserId
      .&. zauthConnId
      .&. jsonRequest @Public.UserUpdate
  document "PUT" "updateSelf" $ do
    Doc.summary "Update your profile"
    Doc.body (Doc.ref Public.modelUserUpdate) $
      Doc.description "JSON body"
    Doc.response 200 "Update successful." Doc.end

  get "/self/name" (continue getUserDisplayNameH) $
    accept "application" "json"
      .&. zauthUserId
  document "GET" "selfName" $ do
    Doc.summary "Get your profile name"
    Doc.returns (Doc.ref Public.modelUserDisplayName)
    Doc.response 200 "Profile name found." Doc.end

  put "/self/email" (continue changeSelfEmailH) $
    zauthUserId
      .&. zauthConnId
      .&. jsonRequest @Public.EmailUpdate
  document "PUT" "changeEmail" $ do
    Doc.summary "Change your email address"
    Doc.body (Doc.ref Public.modelEmailUpdate) $
      Doc.description "JSON body"
    Doc.response 202 "Update accepted and pending activation of the new email." Doc.end
    Doc.response 204 "No update, current and new email address are the same." Doc.end
    Doc.errorResponse invalidEmail
    Doc.errorResponse userKeyExists
    Doc.errorResponse blacklistedEmail
    Doc.errorResponse blacklistedPhone

  put "/self/phone" (continue changePhoneH) $
    zauthUserId
      .&. zauthConnId
      .&. jsonRequest @Public.PhoneUpdate
  document "PUT" "changePhone" $ do
    Doc.summary "Change your phone number"
    Doc.body (Doc.ref Public.modelPhoneUpdate) $
      Doc.description "JSON body"
    Doc.response 202 "Update accepted and pending activation of the new phone number." Doc.end
    Doc.errorResponse userKeyExists

  head "/self/password" (continue checkPasswordExistsH) $
    zauthUserId
  document "HEAD" "checkPassword" $ do
    Doc.summary "Check that your password is set"
    Doc.response 200 "Password is set." Doc.end
    Doc.response 404 "Password is not set." Doc.end

  put "/self/password" (continue changePasswordH) $
    zauthUserId
      .&. jsonRequest @Public.PasswordChange
  document "PUT" "changePassword" $ do
    Doc.summary "Change your password"
    Doc.body (Doc.ref Public.modelChangePassword) $
      Doc.description "JSON body"
    Doc.response 200 "Password changed." Doc.end
    Doc.errorResponse badCredentials
    Doc.errorResponse noIdentity

  put "/self/locale" (continue changeLocaleH) $
    zauthUserId
      .&. zauthConnId
      .&. jsonRequest @Public.LocaleUpdate
  document "PUT" "changeLocale" $ do
    Doc.summary "Change your locale"
    Doc.body (Doc.ref Public.modelChangeLocale) $
      Doc.description "JSON body"
    Doc.response 200 "Locale changed." Doc.end

  -- This endpoint can lead to the following events being sent:
  -- - UserUpdated event to contacts of self
  put "/self/handle" (continue changeHandleH) $
    zauthUserId
      .&. zauthConnId
      .&. jsonRequest @Public.HandleUpdate
  document "PUT" "changeHandle" $ do
    Doc.summary "Change your handle"
    Doc.body (Doc.ref Public.modelChangeHandle) $
      Doc.description "JSON body"
    Doc.errorResponse handleExists
    Doc.errorResponse invalidHandle
    Doc.response 200 "Handle changed." Doc.end

  -- This endpoint can lead to the following events being sent:
  -- - UserIdentityRemoved event to self
  delete "/self/phone" (continue removePhoneH) $
    zauthUserId
      .&. zauthConnId
  document "DELETE" "removePhone" $ do
    Doc.summary "Remove your phone number."
    Doc.notes
      "Your phone number can only be removed if you also have an \
      \email address and a password."
    Doc.response 200 "Phone number removed." Doc.end
    Doc.errorResponse lastIdentity
    Doc.errorResponse noPassword

  -- This endpoint can lead to the following events being sent:
  -- - UserIdentityRemoved event to self
  delete "/self/email" (continue removeEmailH) $
    zauthUserId
      .&. zauthConnId
  document "DELETE" "removeEmail" $ do
    Doc.summary "Remove your email address."
    Doc.notes
      "Your email address can only be removed if you also have a \
      \phone number."
    Doc.response 200 "Email address removed." Doc.end
    Doc.errorResponse lastIdentity

  -- This endpoint can lead to the following events being sent:
  -- - UserDeleted event to contacts of self
  -- - MemberLeave event to members for all conversations the user was in (via galley)
  delete "/self" (continue deleteUserH) $
    zauthUserId
      .&. jsonRequest @Public.DeleteUser
      .&. accept "application" "json"
  document "DELETE" "deleteUser" $ do
    Doc.summary "Initiate account deletion."
    Doc.notes
      "If the account has a verified identity, a verification \
      \code is sent and needs to be confirmed to authorise the \
      \deletion. If the account has no verified identity but a \
      \password, it must be provided. If password is correct, or if neither \
      \a verified identity nor a password exists, account deletion \
      \is scheduled immediately."
    Doc.body (Doc.ref Public.modelDelete) $
      Doc.description "JSON body"
    Doc.response 202 "Deletion is pending verification with a code." Doc.end
    Doc.response 200 "Deletion is initiated." Doc.end
    Doc.errorResponse badCredentials
    Doc.errorResponse missingAuthError

  -- TODO put  where?

  -- This endpoint can lead to the following events being sent:
  -- UserDeleted event to contacts of deleted user
  -- MemberLeave event to members for all conversations the user was in (via galley)
  post "/delete" (continue verifyDeleteUserH) $
    jsonRequest @Public.VerifyDeleteUser
      .&. accept "application" "json"
  document "POST" "verifyDeleteUser" $ do
    Doc.summary "Verify account deletion with a code."
    Doc.body (Doc.ref Public.modelVerifyDelete) $
      Doc.description "JSON body"
    Doc.response 200 "Deletion is initiated." Doc.end
    Doc.errorResponse invalidCode

  -- Connection API -----------------------------------------------------

  -- This endpoint can lead to the following events being sent:
  -- - ConnectionUpdated event to self and other, if any side's connection state changes
  -- - MemberJoin event to self and other, if joining an existing connect conversation (via galley)
  -- - ConvCreate event to self, if creating a connect conversation (via galley)
  -- - ConvConnect event to self, in some cases (via galley),
  --   for details see 'Galley.API.Create.createConnectConversation'
  post "/connections" (continue createConnectionH) $
    accept "application" "json"
      .&. zauthUserId
      .&. zauthConnId
      .&. jsonRequest @Public.ConnectionRequest
  document "POST" "createConnection" $ do
    Doc.summary "Create a connection to another user."
    Doc.notes $
      "You can have no more than "
        <> Text.pack (show (setUserMaxConnections $ optSettings o))
        <> " connections in accepted or sent state."
    Doc.body (Doc.ref Public.modelConnectionRequest) $
      Doc.description "JSON body"
    Doc.returns (Doc.ref Public.modelConnection)
    Doc.response 200 "The connection exists." Doc.end
    Doc.response 201 "The connection was created." Doc.end
    Doc.errorResponse connectionLimitReached
    Doc.errorResponse invalidUser
    Doc.errorResponse noIdentity

  get "/connections" (continue listConnectionsH) $
    accept "application" "json"
      .&. zauthUserId
      .&. opt (query "start")
      .&. def (unsafeRange 100) (query "size")
  document "GET" "connections" $ do
    Doc.summary "List the connections to other users."
    Doc.parameter Doc.Query "start" Doc.string' $ do
      Doc.description "User ID to start from"
      Doc.optional
    Doc.parameter Doc.Query "size" Doc.int32' $ do
      Doc.description "Number of results to return (default 100, max 500)."
      Doc.optional
    Doc.returns (Doc.ref Public.modelConnectionList)
    Doc.response 200 "List of connections" Doc.end

  -- This endpoint can lead to the following events being sent:
  -- - ConnectionUpdated event to self and other, if their connection states change
  --
  -- When changing the connection state to Sent or Accepted, this can cause events to be sent
  -- when joining the connect conversation:
  -- - MemberJoin event to self and other (via galley)
  put "/connections/:id" (continue updateConnectionH) $
    accept "application" "json"
      .&. zauthUserId
      .&. zauthConnId
      .&. capture "id"
      .&. jsonRequest @Public.ConnectionUpdate
  document "PUT" "updateConnection" $ do
    Doc.summary "Update a connection."
    Doc.parameter Doc.Path "id" Doc.bytes' $
      Doc.description "User ID"
    Doc.body (Doc.ref Public.modelConnectionUpdate) $
      Doc.description "JSON body"
    Doc.returns (Doc.ref Public.modelConnection)
    Doc.response 200 "Connection updated." Doc.end
    Doc.response 204 "No change." Doc.end
    Doc.errorResponse connectionLimitReached
    Doc.errorResponse invalidTransition
    Doc.errorResponse notConnected
    Doc.errorResponse invalidUser

  get "/connections/:id" (continue getConnectionH) $
    accept "application" "json"
      .&. zauthUserId
      .&. capture "id"
  document "GET" "connection" $ do
    Doc.summary "Get an existing connection to another user."
    Doc.parameter Doc.Path "id" Doc.bytes' $
      Doc.description "User ID"
    Doc.returns (Doc.ref Public.modelConnection)
    Doc.response 200 "Connection" Doc.end

  -- User Client API ----------------------------------------------------
  -- TODO: another one?

  -- This endpoint can lead to the following events being sent:
  -- - ClientAdded event to self
  -- - ClientRemoved event to self, if removing old clients due to max number
  post "/clients" (continue addClientH) $
    jsonRequest @Public.NewClient
      .&. zauthUserId
      .&. zauthConnId
      .&. opt (header "X-Forwarded-For")
      .&. accept "application" "json"
  document "POST" "registerClient" $ do
    Doc.summary "Register a new client."
    Doc.body (Doc.ref Public.modelNewClient) $
      Doc.description "JSON body"
    Doc.returns (Doc.ref Public.modelClient)
    Doc.response 200 "Client" Doc.end
    Doc.errorResponse tooManyClients
    Doc.errorResponse missingAuthError
    Doc.errorResponse malformedPrekeys

  put "/clients/:client" (continue updateClientH) $
    jsonRequest @Public.UpdateClient
      .&. zauthUserId
      .&. capture "client"
      .&. accept "application" "json"
  document "PUT" "updateClient" $ do
    Doc.summary "Update a registered client."
    Doc.parameter Doc.Path "client" Doc.bytes' $
      Doc.description "Client ID"
    Doc.body (Doc.ref Public.modelUpdateClient) $
      Doc.description "JSON body"
    Doc.response 200 "Client updated." Doc.end
    Doc.errorResponse malformedPrekeys

  -- This endpoint can lead to the following events being sent:
  -- - ClientRemoved event to self
  delete "/clients/:client" (continue rmClientH) $
    jsonRequest @Public.RmClient
      .&. zauthUserId
      .&. zauthConnId
      .&. capture "client"
      .&. accept "application" "json"
  document "DELETE" "deleteClient" $ do
    Doc.summary "Delete an existing client."
    Doc.parameter Doc.Path "client" Doc.bytes' $
      Doc.description "Client ID"
    Doc.body (Doc.ref Public.modelDeleteClient) $
      Doc.description "JSON body"
    Doc.response 200 "Client deleted." Doc.end

  get "/clients" (continue listClientsH) $
    zauthUserId
      .&. accept "application" "json"
  document "GET" "listClients" $ do
    Doc.summary "List the registered clients."
    Doc.returns (Doc.array (Doc.ref Public.modelClient))
    Doc.response 200 "List of clients" Doc.end

  get "/clients/:client" (continue getClientH) $
    zauthUserId
      .&. capture "client"
      .&. accept "application" "json"
  document "GET" "getClients" $ do
    Doc.summary "Get a registered client by ID."
    Doc.parameter Doc.Path "client" Doc.bytes' $
      Doc.description "Client ID"
    Doc.returns (Doc.ref Public.modelClient)
    Doc.response 200 "Client" Doc.end

  get "/clients/:client/prekeys" (continue listPrekeyIdsH) $
    zauthUserId
      .&. capture "client"
      .&. accept "application" "json"
  document "GET" "listPrekeyIds" $ do
    Doc.summary "List the remaining prekey IDs of a client."
    Doc.parameter Doc.Path "client" Doc.bytes' $
      Doc.description "Client ID"
    Doc.returns (Doc.array Doc.string')
    Doc.response 200 "List of remaining prekey IDs." Doc.end

  -- Properties API -----------------------------------------------------

  -- This endpoint can lead to the following events being sent:
  -- - PropertySet event to self
  put "/properties/:key" (continue setPropertyH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "key"
      .&. jsonRequest @Public.PropertyValue
  document "PUT" "setProperty" $ do
    Doc.summary "Set a user property."
    Doc.parameter Doc.Path "key" Doc.string' $
      Doc.description "Property key"
    Doc.body (Doc.ref Public.modelPropertyValue) $
      Doc.description "JSON body"
    Doc.response 200 "Property set." Doc.end

  -- This endpoint can lead to the following events being sent:
  -- - PropertyDeleted event to self
  delete "/properties/:key" (continue deletePropertyH) $
    zauthUserId
      .&. zauthConnId
      .&. capture "key"
  document "DELETE" "deleteProperty" $ do
    Doc.summary "Delete a property."
    Doc.parameter Doc.Path "key" Doc.string' $
      Doc.description "Property key"
    Doc.response 200 "Property deleted." Doc.end

  -- This endpoint can lead to the following events being sent:
  -- - PropertiesCleared event to self
  delete "/properties" (continue clearPropertiesH) $
    zauthUserId
      .&. zauthConnId
  document "DELETE" "clearProperties" $ do
    Doc.summary "Clear all properties."
    Doc.response 200 "Properties cleared." Doc.end

  get "/properties/:key" (continue getPropertyH) $
    zauthUserId
      .&. capture "key"
      .&. accept "application" "json"
  document "GET" "getProperty" $ do
    Doc.summary "Get a property value."
    Doc.parameter Doc.Path "key" Doc.string' $
      Doc.description "Property key"
    Doc.returns (Doc.ref Public.modelPropertyValue)
    Doc.response 200 "The property value." Doc.end

  get "/properties" (continue listPropertyKeysH) $
    zauthUserId
      .&. accept "application" "json"
  document "GET" "listPropertyKeys" $ do
    Doc.summary "List all property keys."
    Doc.returns (Doc.array Doc.string')
    Doc.response 200 "List of property keys." Doc.end

  get "/properties-values" (continue listPropertyKeysAndValuesH) $
    zauthUserId
      .&. accept "application" "json"
  document "GET" "listPropertyKeysAndValues" $ do
    Doc.summary "List all properties with key and value."
    Doc.returns (Doc.ref Public.modelPropertyDictionary)
    Doc.response 200 "Object with properties as attributes." Doc.end

  -- TODO: put delete here, too?
  -- /register, /activate, /password-reset ----------------------------------

  -- docs/reference/user/registration.md {#RefRegistration}
  --
  -- This endpoint can lead to the following events being sent:
  -- - UserActivated event to created user, if it is a team invitation or user has an SSO ID
  -- - UserIdentityUpdated event to created user, if email code or phone code is provided
  post "/register" (continue createUserH) $
    accept "application" "json"
      .&. jsonRequest @Public.NewUserPublic
  document "POST" "register" $ do
    Doc.summary "Register a new user."
    Doc.notes
      "If the environment where the registration takes \
      \place is private and a registered email address or phone \
      \number is not whitelisted, a 403 error is returned."
    Doc.body (Doc.ref Public.modelNewUser) $
      Doc.description "JSON body"
    -- FUTUREWORK: I think this should be 'Doc.self' instead of 'user'
    Doc.returns (Doc.ref Public.modelUser)
    Doc.response 201 "User created and pending activation." Doc.end
    Doc.errorResponse whitelistError
    Doc.errorResponse invalidInvitationCode
    Doc.errorResponse missingIdentity
    Doc.errorResponse userKeyExists
    Doc.errorResponse activationCodeNotFound
    Doc.errorResponse blacklistedEmail
    Doc.errorResponse blacklistedPhone

  -- This endpoint can lead to the following events being sent:
  -- - UserActivated event to the user, if account gets activated
  -- - UserIdentityUpdated event to the user, if email or phone get activated
  get "/activate" (continue activateH) $
    query "key"
      .&. query "code"
  document "GET" "activate" $ do
    Doc.summary "Activate (i.e. confirm) an email address or phone number."
    Doc.notes "See also 'POST /activate' which has a larger feature set."
    Doc.parameter Doc.Query "key" Doc.bytes' $
      Doc.description "Activation key"
    Doc.parameter Doc.Query "code" Doc.bytes' $
      Doc.description "Activation code"
    Doc.returns (Doc.ref Public.modelActivationResponse)
    Doc.response 200 "Activation successful." Doc.end
    Doc.response 204 "A recent activation was already successful." Doc.end
    Doc.errorResponse activationCodeNotFound

  -- docs/reference/user/activation.md {#RefActivationSubmit}
  --
  -- This endpoint can lead to the following events being sent:
  -- - UserActivated event to the user, if account gets activated
  -- - UserIdentityUpdated event to the user, if email or phone get activated
  post "/activate" (continue activateKeyH) $
    accept "application" "json"
      .&. jsonRequest @Public.Activate
  document "POST" "activate" $ do
    Doc.summary "Activate (i.e. confirm) an email address or phone number."
    Doc.notes
      "Activation only succeeds once and the number of \
      \failed attempts for a valid key is limited."
    Doc.body (Doc.ref Public.modelActivate) $
      Doc.description "JSON body"
    Doc.returns (Doc.ref Public.modelActivationResponse)
    Doc.response 200 "Activation successful." Doc.end
    Doc.response 204 "A recent activation was already successful." Doc.end
    Doc.errorResponse activationCodeNotFound

  -- docs/reference/user/activation.md {#RefActivationRequest}
  post "/activate/send" (continue sendActivationCodeH) $
    jsonRequest @Public.SendActivationCode
  document "POST" "sendActivationCode" $ do
    Doc.summary "Send (or resend) an email or phone activation code."
    Doc.body (Doc.ref Public.modelSendActivationCode) $
      Doc.description "JSON body"
    Doc.response 200 "Activation code sent." Doc.end
    Doc.errorResponse invalidEmail
    Doc.errorResponse invalidPhone
    Doc.errorResponse userKeyExists
    Doc.errorResponse blacklistedEmail
    Doc.errorResponse blacklistedPhone
    Doc.errorResponse (customerExtensionBlockedDomain (either undefined id $ mkDomain "example.com"))

  post "/password-reset" (continue beginPasswordResetH) $
    accept "application" "json"
      .&. jsonRequest @Public.NewPasswordReset
  document "POST" "beginPasswordReset" $ do
    Doc.summary "Initiate a password reset."
    Doc.body (Doc.ref Public.modelNewPasswordReset) $
      Doc.description "JSON body"
    Doc.response 201 "Password reset code created and sent by email." Doc.end
    Doc.errorResponse invalidPwResetKey
    Doc.errorResponse duplicatePwResetCode

  post "/password-reset/complete" (continue completePasswordResetH) $
    accept "application" "json"
      .&. jsonRequest @Public.CompletePasswordReset
  document "POST" "completePasswordReset" $ do
    Doc.summary "Complete a password reset."
    Doc.body (Doc.ref Public.modelCompletePasswordReset) $
      Doc.description "JSON body"
    Doc.response 200 "Password reset successful." Doc.end
    Doc.errorResponse invalidPwResetCode

  post "/password-reset/:key" (continue deprecatedCompletePasswordResetH) $
    accept "application" "json"
      .&. capture "key"
      .&. jsonRequest @Public.PasswordReset
  document "POST" "deprecatedCompletePasswordReset" $ do
    Doc.deprecated
    Doc.summary "Complete a password reset."
    Doc.notes "DEPRECATED: Use 'POST /password-reset/complete'."

  post "/onboarding/v3" (continue deprecatedOnboardingH) $
    accept "application" "json"
      .&. zauthUserId
      .&. jsonRequest @Value
  document "POST" "onboardingV3" $ do
    Doc.deprecated
    Doc.summary "Upload contacts and invoke matching."
    Doc.notes
      "DEPRECATED: the feature has been turned off, the end-point does \
      \nothing and always returns '{\"results\":[],\"auto-connects\":[]}'."

  Provider.routesPublic
  Auth.routesPublic
  Search.routesPublic
  Team.routesPublic
  TURN.routesPublic

apiDocs :: Opts -> Routes Doc.ApiBuilder Handler ()
apiDocs o = do
  get
    "/users/api-docs"
    ( \(_ ::: url) k ->
        let doc = mkSwaggerApi (decodeLatin1 url) Public.Swagger.models (sitemap o)
         in k $ json doc
    )
    $ accept "application" "json"
      .&. query "base_url"

---------------------------------------------------------------------------
-- Handlers

setPropertyH :: UserId ::: ConnId ::: Public.PropertyKey ::: JsonRequest Public.PropertyValue -> Handler Response
setPropertyH (u ::: c ::: k ::: req) = do
  propkey <- safeParsePropertyKey k
  propval <- safeParsePropertyValue (lazyRequestBody (fromJsonRequest req))
  empty <$ setProperty u c propkey propval

setProperty :: UserId -> ConnId -> Public.PropertyKey -> Public.PropertyValue -> Handler ()
setProperty u c propkey propval = do
  API.setProperty u c propkey propval !>> propDataError

safeParsePropertyKey :: Public.PropertyKey -> Handler Public.PropertyKey
safeParsePropertyKey k = do
  maxKeyLen <- fromMaybe defMaxKeyLen <$> view (settings . propertyMaxKeyLen)
  let keyText = Ascii.toText (Public.propertyKeyName k)
  when (Text.compareLength keyText (fromIntegral maxKeyLen) == GT) $
    throwStd propertyKeyTooLarge
  pure k

-- | Parse a 'PropertyValue' from a bytestring.  This is different from 'FromJSON' in that
-- checks the byte size of the input, and fails *without consuming all of it* if that size
-- exceeds the settings.
safeParsePropertyValue :: IO Lazy.ByteString -> Handler Public.PropertyValue
safeParsePropertyValue lreqbody = do
  maxValueLen <- fromMaybe defMaxValueLen <$> view (settings . propertyMaxValueLen)
  lbs <- Lazy.take (maxValueLen + 1) <$> liftIO lreqbody
  unless (Lazy.length lbs <= maxValueLen) $
    throwStd propertyValueTooLarge
  hoistEither $ fmapL (StdError . badRequest . pack) (eitherDecode lbs)

deletePropertyH :: UserId ::: ConnId ::: Public.PropertyKey -> Handler Response
deletePropertyH (u ::: c ::: k) = lift (API.deleteProperty u c k) >> return empty

clearPropertiesH :: UserId ::: ConnId -> Handler Response
clearPropertiesH (u ::: c) = lift (API.clearProperties u c) >> return empty

getPropertyH :: UserId ::: Public.PropertyKey ::: JSON -> Handler Response
getPropertyH (u ::: k ::: _) = do
  val <- lift $ API.lookupProperty u k
  return $ case val of
    Nothing -> setStatus status404 empty
    Just v -> json (v :: Public.PropertyValue)

listPropertyKeysH :: UserId ::: JSON -> Handler Response
listPropertyKeysH (u ::: _) = do
  keys <- lift (API.lookupPropertyKeys u)
  pure $ json (keys :: [Public.PropertyKey])

listPropertyKeysAndValuesH :: UserId ::: JSON -> Handler Response
listPropertyKeysAndValuesH (u ::: _) = do
  keysAndVals <- lift (API.lookupPropertyKeysAndValues u)
  pure $ json (keysAndVals :: Public.PropertyKeysAndValues)

getPrekeyH :: OpaqueUserId ::: ClientId ::: JSON -> Handler Response
getPrekeyH (u ::: c ::: _) = do
  getPrekey u c <&> \case
    Just pk -> json pk
    Nothing -> setStatus status404 empty

getPrekey :: OpaqueUserId -> ClientId -> Handler (Maybe Public.ClientPrekey)
getPrekey u c = lift $ do
  resolvedUserId <- resolveOpaqueUserId u
  API.claimPrekey resolvedUserId c

getPrekeyBundleH :: OpaqueUserId ::: JSON -> Handler Response
getPrekeyBundleH (u ::: _) = json <$> getPrekeyBundle u

getPrekeyBundle :: OpaqueUserId -> Handler Public.PrekeyBundle
getPrekeyBundle u = lift $ do
  resolvedUserId <- resolveOpaqueUserId u
  API.claimPrekeyBundle resolvedUserId

getMultiPrekeyBundlesH :: JsonRequest Public.UserClients ::: JSON -> Handler Response
getMultiPrekeyBundlesH (req ::: _) = do
  json <$> (getMultiPrekeyBundles =<< parseJsonBody req)

getMultiPrekeyBundles :: Public.UserClients -> Handler (Public.UserClientMap (Maybe Public.Prekey))
getMultiPrekeyBundles body = do
  maxSize <- fromIntegral . setMaxConvSize <$> view settings
  when (Map.size (Public.userClients body) > maxSize) $
    throwStd tooManyClients
  API.claimMultiPrekeyBundles body

addClientH :: JsonRequest Public.NewClient ::: UserId ::: ConnId ::: Maybe IpAddr ::: JSON -> Handler Response
addClientH (req ::: usr ::: con ::: ip ::: _) = do
  new <- parseJsonBody req
  clt <- addClient new usr con ip
  let loc = toByteString' $ Public.clientId clt
  pure . setStatus status201 . addHeader "Location" loc . json $ clt

addClient :: Public.NewClient -> UserId -> ConnId -> Maybe IpAddr -> Handler Public.Client
addClient new usr con ip = do
  -- Users can't add legal hold clients
  when (Public.newClientType new == Public.LegalHoldClientType) $
    throwE (clientError ClientLegalHoldCannotBeAdded)
  API.addClient usr (Just con) (ipAddr <$> ip) new !>> clientError

rmClientH :: JsonRequest Public.RmClient ::: UserId ::: ConnId ::: ClientId ::: JSON -> Handler Response
rmClientH (req ::: usr ::: con ::: clt ::: _) = do
  body <- parseJsonBody req
  empty <$ rmClient body usr con clt

rmClient :: Public.RmClient -> UserId -> ConnId -> ClientId -> Handler ()
rmClient body usr con clt = do
  API.rmClient usr con clt (Public.rmPassword body) !>> clientError

updateClientH :: JsonRequest Public.UpdateClient ::: UserId ::: ClientId ::: JSON -> Handler Response
updateClientH (req ::: usr ::: clt ::: _) = do
  body <- parseJsonBody req
  empty <$ updateClient body usr clt

updateClient :: Public.UpdateClient -> UserId -> ClientId -> Handler ()
updateClient body usr clt = do
  API.updateClient usr clt body !>> clientError

listClientsH :: UserId ::: JSON -> Handler Response
listClientsH (zusr ::: _) =
  json <$> listClients zusr

listClients :: UserId -> Handler [Public.Client]
listClients zusr = do
  API.lookupClients (Local zusr) !>> clientError

getClientH :: UserId ::: ClientId ::: JSON -> Handler Response
getClientH (zusr ::: clt ::: _) =
  getClient zusr clt <&> \case
    Just c -> json c
    Nothing -> setStatus status404 empty

getClient :: UserId -> ClientId -> Handler (Maybe Public.Client)
getClient zusr clientId = do
  API.lookupClient (Local zusr) clientId !>> clientError

getUserClientsH :: OpaqueUserId ::: JSON -> Handler Response
getUserClientsH (user ::: _) =
  json <$> getUserClients user

getUserClients :: OpaqueUserId -> Handler [Public.PubClient]
getUserClients opaqueUserId = do
  resolvedUserId <- lift $ resolveOpaqueUserId opaqueUserId
  API.pubClient <$$> API.lookupClients resolvedUserId !>> clientError

getUserClientH :: OpaqueUserId ::: ClientId ::: JSON -> Handler Response
getUserClientH (user ::: cid ::: _) = do
  maybe (setStatus status404 empty) json <$> getUserClient user cid

getUserClient :: OpaqueUserId -> ClientId -> Handler (Maybe Public.PubClient)
getUserClient opaqueUserId clientId = do
  resolvedUserId <- lift $ resolveOpaqueUserId opaqueUserId
  API.pubClient <$$> API.lookupClient resolvedUserId clientId !>> clientError

getRichInfoH :: UserId ::: UserId ::: JSON -> Handler Response
getRichInfoH (self ::: user ::: _) = do
  json <$> getRichInfo self user

getRichInfo :: UserId -> UserId -> Handler Public.RichInfoAssocList
getRichInfo self user = do
  -- Check that both users exist and the requesting user is allowed to see rich info of the
  -- other user
  selfUser <- ifNothing userNotFound =<< lift (Data.lookupUser self)
  otherUser <- ifNothing userNotFound =<< lift (Data.lookupUser user)
  case (Public.userTeam selfUser, Public.userTeam otherUser) of
    (Just t1, Just t2) | t1 == t2 -> pure ()
    _ -> throwStd insufficientTeamPermissions
  -- Query rich info
  fromMaybe Public.emptyRichInfoAssocList <$> lift (API.lookupRichInfo user)

listPrekeyIdsH :: UserId ::: ClientId ::: JSON -> Handler Response
listPrekeyIdsH (usr ::: clt ::: _) = do
  prekeyIds <- lift (API.lookupPrekeyIds usr clt)
  pure $ json (prekeyIds :: [Public.PrekeyId])

-- docs/reference/user/registration.md {#RefRegistration}
createUserH :: JSON ::: JsonRequest Public.NewUserPublic -> Handler Response
createUserH (_ ::: req) = do
  CreateUserResponse cok loc prof <- createUser =<< parseJsonBody req
  lift . Auth.setResponseCookie cok
    . setStatus status201
    . addHeader "Location" (toByteString' loc)
    $ json prof

data CreateUserResponse
  = CreateUserResponse (Public.Cookie (ZAuth.Token ZAuth.User)) UserId Public.SelfProfile

createUser :: Public.NewUserPublic -> Handler CreateUserResponse
createUser (Public.NewUserPublic new) = do
  API.checkRestrictedUserCreation new !>> newUserError
  for_ (Public.newUserEmail new) $ checkWhitelist . Left
  for_ (Public.newUserPhone new) $ checkWhitelist . Right
  result <- API.createUser new !>> newUserError
  let acc = createdAccount result
  let eac = createdEmailActivation result
  let pac = createdPhoneActivation result
  let epair = (,) <$> (activationKey <$> eac) <*> (activationCode <$> eac)
  let ppair = (,) <$> (activationKey <$> pac) <*> (activationCode <$> pac)
  let newUserLabel = Public.newUserLabel new
  let newUserTeam = Public.newUserTeam new
  let usr = accountUser acc
  let Public.User {userLocale, userDisplayName, userId} = usr
  let userEmail = Public.userEmail usr
  let userPhone = Public.userPhone usr
  lift $ do
    for_ (liftM2 (,) userEmail epair) $ \(e, p) ->
      sendActivationEmail e userDisplayName p (Just userLocale) newUserTeam
    for_ (liftM2 (,) userPhone ppair) $ \(p, c) ->
      sendActivationSms p c (Just userLocale)
    for_ (liftM3 (,,) userEmail (createdUserTeam result) newUserTeam) $ \(e, ct, ut) ->
      sendWelcomeEmail e ct ut (Just userLocale)
  cok <- case acc of
    UserAccount _ Ephemeral -> lift $ Auth.newCookie @ZAuth.User userId Public.SessionCookie newUserLabel
    UserAccount _ _ -> lift $ Auth.newCookie @ZAuth.User userId Public.PersistentCookie newUserLabel
  pure $ CreateUserResponse cok userId (Public.SelfProfile usr)
  where
    sendActivationEmail e u p l mTeamUser
      | Just teamUser <- mTeamUser,
        Public.NewTeamCreator creator <- teamUser,
        let Public.BindingNewTeamUser (Public.BindingNewTeam team) _ = creator =
        sendTeamActivationMail e u p l (fromRange $ team ^. Public.newTeamName)
      | otherwise =
        sendActivationMail e u p l Nothing
    sendWelcomeEmail :: Public.Email -> CreateUserTeam -> Public.NewTeamUser -> Maybe Public.Locale -> AppIO ()
    -- NOTE: Welcome e-mails for the team creator are not dealt by brig anymore
    sendWelcomeEmail e (CreateUserTeam t n) newUser l = case newUser of
      Public.NewTeamCreator _ ->
        return ()
      Public.NewTeamMember _ ->
        Team.sendMemberWelcomeMail e t n l
      Public.NewTeamMemberSSO _ ->
        Team.sendMemberWelcomeMail e t n l

checkUserExistsH :: UserId ::: OpaqueUserId -> Handler Response
checkUserExistsH (self ::: uid) = do
  exists <- checkUserExists self uid
  if exists then return empty else throwStd userNotFound

checkUserExists :: UserId -> OpaqueUserId -> Handler Bool
checkUserExists self opaqueUserId =
  isJust <$> getUser self opaqueUserId

getSelfH :: JSON ::: UserId -> Handler Response
getSelfH (_ ::: self) = do
  json <$> getSelf self

getSelf :: UserId -> Handler Public.SelfProfile
getSelf self = do
  lift (API.lookupSelfProfile self) >>= ifNothing userNotFound

getUserH :: JSON ::: UserId ::: OpaqueUserId -> Handler Response
getUserH (_ ::: self ::: uid) = do
  fmap json . ifNothing userNotFound =<< getUser self uid

getUser :: UserId -> OpaqueUserId -> Handler (Maybe Public.UserProfile)
getUser self opaqueUserId = lift $ do
  resolvedUserId <- resolveOpaqueUserId opaqueUserId
  API.lookupProfile self resolvedUserId

getUserDisplayNameH :: JSON ::: UserId -> Handler Response
getUserDisplayNameH (_ ::: self) = do
  name :: Maybe Public.Name <- lift $ API.lookupName self
  return $ case name of
    Just n -> json $ object ["name" .= n]
    Nothing -> setStatus status404 empty

listUsersH :: JSON ::: UserId ::: Either (List OpaqueUserId) (Range 1 4 (List (OptionallyQualified Handle))) -> Handler Response
listUsersH (_ ::: self ::: qry) =
  toResponse <$> listUsers self qry
  where
    toResponse = \case
      [] -> setStatus status404 empty
      ps -> json ps

listUsers :: UserId -> Either (List OpaqueUserId) (Range 1 4 (List (OptionallyQualified Handle))) -> Handler [Public.UserProfile]
listUsers self = \case
  Left us -> do
    resolvedUserIds <- lift $ traverse resolveOpaqueUserId (fromList us)
    byIds resolvedUserIds
  Right hs -> do
    us <- getIds (fromList $ fromRange hs)
    filterHandleResults self =<< byIds us
  where
    getIds :: [OptionallyQualified Handle] -> Handler [MappedOrLocalId Id.U]
    getIds hs = do
      -- we might be able to do something smarter if the domain is our own
      let (localHandles, _qualifiedHandles) = partitionEithers (map eitherQualifiedOrNot hs)
      localUsers <- catMaybes <$> traverse (lift . API.lookupHandle) localHandles
      -- FUTUREWORK(federation, #1268): resolve qualified handles, too
      pure (Local <$> localUsers)
    byIds :: [MappedOrLocalId Id.U] -> Handler [Public.UserProfile]
    byIds uids = lift $ API.lookupProfiles self uids

newtype GetActivationCodeResp
  = GetActivationCodeResp (Public.ActivationKey, Public.ActivationCode)

instance ToJSON GetActivationCodeResp where
  toJSON (GetActivationCodeResp (k, c)) = object ["key" .= k, "code" .= c]

updateUserH :: UserId ::: ConnId ::: JsonRequest Public.UserUpdate -> Handler Response
updateUserH (uid ::: conn ::: req) = do
  uu <- parseJsonBody req
  lift $ API.updateUser uid conn uu
  return empty

changePhoneH :: UserId ::: ConnId ::: JsonRequest Public.PhoneUpdate -> Handler Response
changePhoneH (u ::: c ::: req) = do
  setStatus status202 empty <$ (changePhone u c =<< parseJsonBody req)

changePhone :: UserId -> ConnId -> Public.PhoneUpdate -> Handler ()
changePhone u _ (Public.puPhone -> phone) = do
  (adata, pn) <- API.changePhone u phone !>> changePhoneError
  loc <- lift $ API.lookupLocale u
  let apair = (activationKey adata, activationCode adata)
  lift $ sendActivationSms pn apair loc

removePhoneH :: UserId ::: ConnId -> Handler Response
removePhoneH (self ::: conn) = do
  API.removePhone self conn !>> idtError
  return empty

removeEmailH :: UserId ::: ConnId -> Handler Response
removeEmailH (self ::: conn) = do
  API.removeEmail self conn !>> idtError
  return empty

checkPasswordExistsH :: UserId -> Handler Response
checkPasswordExistsH self = do
  exists <- lift $ isJust <$> API.lookupPassword self
  return $ if exists then empty else setStatus status404 empty

changePasswordH :: UserId ::: JsonRequest Public.PasswordChange -> Handler Response
changePasswordH (u ::: req) = do
  cp <- parseJsonBody req
  API.changePassword u cp !>> changePwError
  return empty

changeLocaleH :: UserId ::: ConnId ::: JsonRequest Public.LocaleUpdate -> Handler Response
changeLocaleH (u ::: conn ::: req) = do
  l <- parseJsonBody req
  lift $ API.changeLocale u conn l
  return empty

data CheckHandleResp
  = CheckHandleInvalid
  | CheckHandleFound
  | CheckHandleNotFound

checkHandleH :: UserId ::: Text -> Handler Response
checkHandleH (uid ::: hndl) = do
  checkHandle uid hndl >>= \case
    CheckHandleInvalid -> throwE (StdError invalidHandle)
    CheckHandleFound -> pure $ setStatus status200 empty
    CheckHandleNotFound -> pure $ setStatus status404 empty

checkHandle :: UserId -> Text -> Handler CheckHandleResp
checkHandle _ uhandle = do
  handle <- validateHandle uhandle
  owner <- lift $ API.lookupHandle handle
  if
      | isJust owner ->
        -- Handle is taken (=> getHandleInfo will return 200)
        return CheckHandleFound
      | API.isBlacklistedHandle handle ->
        -- Handle is free but cannot be taken
        return CheckHandleInvalid
      | otherwise ->
        -- Handle is free and can be taken
        return CheckHandleNotFound

checkHandlesH :: JSON ::: UserId ::: JsonRequest Public.CheckHandles -> Handler Response
checkHandlesH (_ ::: _ ::: req) = do
  Public.CheckHandles hs num <- parseJsonBody req
  let handles = mapMaybe parseHandle (fromRange hs)
  free <- lift $ API.checkHandles handles (fromRange num)
  return $ json (free :: [Handle])

getHandleInfoH :: JSON ::: UserId ::: Handle -> Handler Response
getHandleInfoH (_ ::: self ::: handle) =
  maybe (setStatus status404 empty) json
    <$> getHandleInfo self handle

-- FUTUREWORK: use 'runMaybeT' to simplify this.
getHandleInfo :: UserId -> Handle -> Handler (Maybe Public.UserHandleInfo)
getHandleInfo self handle = do
  ownerProfile <- do
    -- FUTUREWORK(federation, #1268): resolve qualified handles, too
    maybeOwnerId <- fmap Local <$> (lift $ API.lookupHandle handle)
    case maybeOwnerId of
      Just ownerId -> lift $ API.lookupProfile self ownerId
      Nothing -> return Nothing
  owner <- filterHandleResults self (maybeToList ownerProfile)
  return $ Public.UserHandleInfo . Public.profileId <$> listToMaybe owner

changeHandleH :: UserId ::: ConnId ::: JsonRequest Public.HandleUpdate -> Handler Response
changeHandleH (u ::: conn ::: req) = do
  empty <$ (changeHandle u conn =<< parseJsonBody req)

changeHandle :: UserId -> ConnId -> Public.HandleUpdate -> Handler ()
changeHandle u conn (Public.HandleUpdate h) = do
  handle <- validateHandle h
  API.changeHandle u conn handle !>> changeHandleError

beginPasswordResetH :: JSON ::: JsonRequest Public.NewPasswordReset -> Handler Response
beginPasswordResetH (_ ::: req) = do
  setStatus status201 empty <$ (beginPasswordReset =<< parseJsonBody req)

beginPasswordReset :: Public.NewPasswordReset -> Handler ()
beginPasswordReset (Public.NewPasswordReset target) = do
  checkWhitelist target
  (u, pair) <- API.beginPasswordReset target !>> pwResetError
  loc <- lift $ API.lookupLocale u
  lift $ case target of
    Left email -> sendPasswordResetMail email pair loc
    Right phone -> sendPasswordResetSms phone pair loc

completePasswordResetH :: JSON ::: JsonRequest Public.CompletePasswordReset -> Handler Response
completePasswordResetH (_ ::: req) = do
  Public.CompletePasswordReset {..} <- parseJsonBody req
  API.completePasswordReset cpwrIdent cpwrCode cpwrPassword !>> pwResetError
  return empty

sendActivationCodeH :: JsonRequest Public.SendActivationCode -> Handler Response
sendActivationCodeH req = do
  empty <$ (sendActivationCode =<< parseJsonBody req)

-- docs/reference/user/activation.md {#RefActivationRequest}
-- docs/reference/user/registration.md {#RefRegistration}
sendActivationCode :: Public.SendActivationCode -> Handler ()
sendActivationCode Public.SendActivationCode {..} = do
  checkWhitelist saUserKey
  either customerExtensionCheckBlockedDomains (\_ -> pure ()) saUserKey
  API.sendActivationCode saUserKey saLocale saCall !>> sendActCodeError

-- | If the user presents an email address from a blocked domain, throw an error.
--
-- The tautological constraint in the type signature is added so that once we remove the
-- feature, ghc will guide us here.
customerExtensionCheckBlockedDomains :: (DomainsBlockedForRegistration ~ DomainsBlockedForRegistration) => Public.Email -> Handler ()
customerExtensionCheckBlockedDomains email = do
  mBlockedDomains <- asks (fmap domainsBlockedForRegistration . setCustomerExtensions . view settings)
  for_ mBlockedDomains $ \(DomainsBlockedForRegistration blockedDomains) -> do
    case mkDomain (Public.emailDomain email) of
      Left _ ->
        pure () -- if it doesn't fit the syntax of blocked domains, it is not blocked
      Right domain ->
        when (domain `elem` blockedDomains) $ do
          throwM $ customerExtensionBlockedDomain domain

changeSelfEmailH :: UserId ::: ConnId ::: JsonRequest Public.EmailUpdate -> Handler Response
changeSelfEmailH (u ::: _ ::: req) = do
  email <- Public.euEmail <$> parseJsonBody req
  API.changeSelfEmail u email >>= \case
    ChangeEmailResponseIdempotent -> pure (setStatus status204 empty)
    ChangeEmailResponseNeedsActivation -> pure (setStatus status202 empty)

createConnectionH :: JSON ::: UserId ::: ConnId ::: JsonRequest Public.ConnectionRequest -> Handler Response
createConnectionH (_ ::: self ::: conn ::: req) = do
  cr <- parseJsonBody req
  rs <- API.createConnection self cr conn !>> connError
  return $ case rs of
    ConnectionCreated c -> setStatus status201 $ json (c :: Public.UserConnection)
    ConnectionExists c -> json (c :: Public.UserConnection)

updateConnectionH :: JSON ::: UserId ::: ConnId ::: UserId ::: JsonRequest Public.ConnectionUpdate -> Handler Response
updateConnectionH (_ ::: self ::: conn ::: other ::: req) = do
  newStatus <- Public.cuStatus <$> parseJsonBody req
  mc <- API.updateConnection self other newStatus (Just conn) !>> connError
  return $ case mc of
    Just c -> json (c :: Public.UserConnection)
    Nothing -> setStatus status204 empty

listConnectionsH :: JSON ::: UserId ::: Maybe UserId ::: Range 1 500 Int32 -> Handler Response
listConnectionsH (_ ::: uid ::: start ::: size) =
  json @Public.UserConnectionList
    <$> lift (API.lookupConnections uid start size)

getConnectionH :: JSON ::: UserId ::: UserId -> Handler Response
getConnectionH (_ ::: uid ::: uid') = lift $ do
  conn <- API.lookupConnection uid uid'
  return $ case conn of
    Just c -> json (c :: Public.UserConnection)
    Nothing -> setStatus status404 empty

deleteUserH :: UserId ::: JsonRequest Public.DeleteUser ::: JSON -> Handler Response
deleteUserH (u ::: r ::: _) = do
  body <- parseJsonBody r
  res <- API.deleteUser u (Public.deleteUserPassword body) !>> deleteUserError
  return $ case res of
    Nothing -> setStatus status200 empty
    Just ttl -> setStatus status202 (json (Public.DeletionCodeTimeout ttl))

verifyDeleteUserH :: JsonRequest Public.VerifyDeleteUser ::: JSON -> Handler Response
verifyDeleteUserH (r ::: _) = do
  body <- parseJsonBody r
  API.verifyDeleteUser body !>> deleteUserError
  return (setStatus status200 empty)

-- activation

data ActivationRespWithStatus
  = ActivationResp Public.ActivationResponse
  | ActivationRespDryRun
  | ActivationRespPass
  | ActivationRespSuccessNoIdent

respFromActivationRespWithStatus :: ActivationRespWithStatus -> Response
respFromActivationRespWithStatus = \case
  ActivationResp aresp -> json aresp
  ActivationRespDryRun -> empty
  ActivationRespPass -> setStatus status204 empty
  ActivationRespSuccessNoIdent -> empty

-- docs/reference/user/activation.md {#RefActivationSubmit}
activateKeyH :: JSON ::: JsonRequest Public.Activate -> Handler Response
activateKeyH (_ ::: req) = do
  activationRequest <- parseJsonBody req
  respFromActivationRespWithStatus <$> activate activationRequest

activateH :: Public.ActivationKey ::: Public.ActivationCode -> Handler Response
activateH (k ::: c) = do
  let activationRequest = Public.Activate (Public.ActivateKey k) c False
  respFromActivationRespWithStatus <$> activate activationRequest

activate :: Public.Activate -> Handler ActivationRespWithStatus
activate (Public.Activate tgt code dryrun)
  | dryrun = do
    API.preverify tgt code !>> actError
    return $ ActivationRespDryRun
  | otherwise = do
    result <- API.activate tgt code Nothing !>> actError
    return $ case result of
      ActivationSuccess ident first -> respond ident first
      ActivationPass -> ActivationRespPass
  where
    respond (Just ident) first = ActivationResp $ Public.ActivationResponse ident first
    respond Nothing _ = ActivationRespSuccessNoIdent

-- Deprecated

deprecatedOnboardingH :: JSON ::: UserId ::: JsonRequest Value -> Handler Response
deprecatedOnboardingH (_ ::: _ ::: _) = pure $ json DeprecatedMatchingResult

data DeprecatedMatchingResult = DeprecatedMatchingResult

instance ToJSON DeprecatedMatchingResult where
  toJSON DeprecatedMatchingResult =
    object
      [ "results" .= ([] :: [()]),
        "auto-connects" .= ([] :: [()])
      ]

deprecatedCompletePasswordResetH :: JSON ::: Public.PasswordResetKey ::: JsonRequest Public.PasswordReset -> Handler Response
deprecatedCompletePasswordResetH (_ ::: k ::: req) = do
  pwr <- parseJsonBody req
  API.completePasswordReset
    (Public.PasswordResetIdentityKey k)
    (Public.pwrCode pwr)
    (Public.pwrPassword pwr)
    !>> pwResetError
  return empty

-- Utilities

validateHandle :: Text -> Handler Handle
validateHandle = maybe (throwE (StdError invalidHandle)) return . parseHandle

ifNothing :: Utilities.Error -> Maybe a -> Handler a
ifNothing e = maybe (throwStd e) return

-- | Checks search permissions and filters accordingly
filterHandleResults :: UserId -> [Public.UserProfile] -> Handler [Public.UserProfile]
filterHandleResults searchingUser us = do
  sameTeamSearchOnly <- fromMaybe False <$> view (settings . searchSameTeamOnly)
  if sameTeamSearchOnly
    then do
      fromTeam <- lift $ Data.lookupUserTeam searchingUser
      return $ case fromTeam of
        Just team -> filter (\x -> Public.profileTeam x == Just team) us
        Nothing -> us
    else return us

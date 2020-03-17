{-# LANGUAGE RecordWildCards #-}

module Brig.API
  ( sitemap,
  )
where

import qualified Brig.API.Client as API
import qualified Brig.API.Connection as API
import Brig.API.Error
import Brig.API.Handler
import qualified Brig.API.Properties as API
import Brig.API.Types
import qualified Brig.API.User as API
import Brig.API.Util (resolveOpaqueUserId)
import Brig.App
import qualified Brig.Data.User as Data
import Brig.Options hiding (internalEvents, sesQueue)
import qualified Brig.Provider.API as Provider
import qualified Brig.TURN.API as TURN
import qualified Brig.Team.API as Team
import qualified Brig.Team.Email as Team
import qualified Brig.Team.Util as Team
import Brig.Types
import Brig.Types.Intra
import qualified Brig.Types.Swagger as Doc
import Brig.Types.Team.LegalHold (LegalHoldClientRequest (..))
import Brig.Types.User (NewUserPublic (NewUserPublic))
import Brig.Types.User.Auth
import qualified Brig.User.API.Auth as Auth
import qualified Brig.User.API.Search as Search
import qualified Brig.User.Auth.Cookie as Auth
import Brig.User.Email
import Brig.User.Phone
import Control.Error hiding (bool)
import Control.Lens ((^.), view)
import Data.Aeson hiding (json)
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as Lazy
import Data.Handle (Handle, parseHandle)
import Data.Id as Id
import Data.IdMapping (MappedOrLocalId (Local))
import qualified Data.List1 as List1
import qualified Data.Map.Strict as Map
import Data.Misc ((<$$>), IpAddr (..))
import Data.Qualified (OptionallyQualified, eitherQualifiedOrNot)
import Data.Range
import qualified Data.Set as Set
import qualified Data.Swagger.Build.Api as Doc
import qualified Data.Text as Text
import qualified Data.Text.Ascii as Ascii
import Data.Text.Encoding (decodeLatin1)
import Data.Text.Lazy (pack)
import qualified Data.ZAuth.Token as ZAuth
import Galley.Types (UserClientMap (..), UserClients (..))
import qualified Galley.Types.Swagger as Doc
import qualified Galley.Types.Teams as Team
import Imports hiding (head)
import Network.HTTP.Types.Status
import Network.Wai (Response, lazyRequestBody)
import Network.Wai.Predicate hiding (result, setStatus)
import Network.Wai.Routing
import Network.Wai.Utilities
import qualified Network.Wai.Utilities as Utilities
import Network.Wai.Utilities.Response (json)
import Network.Wai.Utilities.Swagger (document, mkSwaggerApi)
import qualified Network.Wai.Utilities.Swagger as Doc

---------------------------------------------------------------------------
-- Sitemap

sitemap :: Opts -> Routes Doc.ApiBuilder Handler ()
sitemap o = do
  -- Internal ---------------------------------------------------------------

  get "/i/status" (continue $ const $ return empty) true
  head "/i/status" (continue $ const $ return empty) true
  -- ConnectionUpdated event to self and users connecting with
  --
  -- This will cause events to be sent from Galley:
  -- for connect conversations that did not exist before:
  --   via galley: ConvCreate EdConversation event to self
  -- if others didn't join a connect conversation with self before:
  --   via galley: ConvConnect EdConnect event to self
  --   via galley: MemberJoin EdMembersJoin event to you and other
  post "/i/users/:uid/auto-connect" (continue (autoConnectH N E)) $
    accept "application" "json"
      .&. capture "uid"
      .&. opt (header "Z-Connection")
      .&. jsonRequest @UserSet
  -- UserActivated event to self, if user is team user
  -- UserActivated event to self, if user has SSO ID
  -- UserIdentityUpdated event to self, if email or phone get activated (should always happen)
  post "/i/users" (continue (createUserNoVerifyH E)) $
    accept "application" "json"
      .&. jsonRequest @NewUser
  put "/i/self/email" (continue changeSelfEmailNoSendH) $
    header "Z-User"
      .&. jsonRequest @EmailUpdate
  -- when internal event is handled asynchronously:
  --   UserDeleted event to contacts
  --   via galley: MemberLeave EdMembersLeave event to members for all conversations the user was in
  delete "/i/users/:uid" (continue (deleteUserNoVerifyH N E)) $
    capture "uid"
  get "/i/users/connections-status" (continue deprecatedGetConnectionsStatusH) $
    query "users"
      .&. opt (query "filter")
  post "/i/users/connections-status" (continue getConnectionsStatusH) $
    accept "application" "json"
      .&. jsonRequest @ConnectionsStatusRequest
      .&. opt (query "filter")
  get "/i/users" (continue listActivatedAccountsH) $
    accept "application" "json"
      .&. (param "ids" ||| param "handles")
  get "/i/users" (continue listAccountsByIdentityH) $
    accept "application" "json"
      .&. (param "email" ||| param "phone")
  put "/i/users/:uid/status" (continue changeAccountStatusH) $
    capture "uid"
      .&. jsonRequest @AccountStatusUpdate
  get "/i/users/:uid/status" (continue getAccountStatusH) $
    accept "application" "json"
      .&. capture "uid"
  get "/i/users/:uid/contacts" (continue getContactListH) $
    accept "application" "json"
      .&. capture "uid"
  get "/i/users/activation-code" (continue getActivationCodeH) $
    accept "application" "json"
      .&. (param "email" ||| param "phone")
  get "/i/users/password-reset-code" (continue getPasswordResetCodeH) $
    accept "application" "json"
      .&. (param "email" ||| param "phone")
  -- UserIdentityRemoved event to self
  post "/i/users/revoke-identity" (continue (revokeIdentityH E)) $
    param "email" ||| param "phone"
  head "/i/users/blacklist" (continue checkBlacklistH) $
    param "email" ||| param "phone"
  delete "/i/users/blacklist" (continue deleteFromBlacklistH) $
    param "email" ||| param "phone"
  post "/i/users/blacklist" (continue addBlacklistH) $
    param "email" ||| param "phone"
  -- given a phone number (or phone number prefix), see whether
  -- it is blocked via a prefix (and if so, via which specific prefix)
  get "/i/users/phone-prefixes/:prefix" (continue getPhonePrefixesH) $
    capture "prefix"
  delete "/i/users/phone-prefixes/:prefix" (continue deleteFromPhonePrefixH) $
    capture "prefix"
  post "/i/users/phone-prefixes" (continue addPhonePrefixH) $
    accept "application" "json"
      .&. jsonRequest @ExcludedPrefix
  -- is :uid not team owner, or there are other team owners?
  get "/i/users/:uid/can-be-deleted/:tid" (continue canBeDeletedH) $
    capture "uid"
      .&. capture "tid"
  -- is :uid team owner (the only one or one of several)?
  get "/i/users/:uid/is-team-owner/:tid" (continue isTeamOwnerH) $
    capture "uid"
      .&. capture "tid"
  put "/i/users/:uid/sso-id" (continue updateSSOIdH) $
    capture "uid"
      .&. accept "application" "json"
      .&. jsonRequest @UserSSOId
  put "/i/users/:uid/managed-by" (continue updateManagedByH) $
    capture "uid"
      .&. accept "application" "json"
      .&. jsonRequest @ManagedByUpdate
  put "/i/users/:uid/rich-info" (continue updateRichInfoH) $
    capture "uid"
      .&. accept "application" "json"
      .&. jsonRequest @RichInfoUpdate
  post "/i/clients" (continue internalListClientsH) $
    accept "application" "json"
      .&. jsonRequest @UserSet
  -- ClientAdded event to self
  -- UserLegalHoldEnabled event to contacts, if client is legalhold
  -- ClientRemoved event to self, if removing old clients
  post "/i/clients/:uid" (continue (addClientInternalH E)) $
    capture "uid"
      .&. jsonRequest @NewClient
      .&. opt (header "Z-Connection")
      .&. accept "application" "json"
  -- LegalHoldClientRequested event to contacts
  post "/i/clients/legalhold/:uid/request" (continue (legalHoldClientRequestedH E)) $
    capture "uid"
      .&. jsonRequest @LegalHoldClientRequest
      .&. accept "application" "json"
  -- ClientRemoved event to self
  -- UserLegalHoldDisabled event to contacts
  delete "/i/clients/legalhold/:uid" (continue (removeLegalHoldClientH E)) $
    capture "uid"
      .&. accept "application" "json"
  -- /users -----------------------------------------------------------------

  get
    "/users/api-docs"
    ( \(_ ::: url) k ->
        let doc = mkSwaggerApi (decodeLatin1 url) Doc.brigModels (sitemap o)
         in k $ json doc
    )
    $ accept "application" "json"
      .&. query "base_url"
  ---

  -- if the user is expired, an internal event is handled asynchronously:
  --   UserDeleted event to contacts
  --   via galley: MemberLeave EdMembersLeave event to members for all conversations the user was in
  head "/users/:uid" (continue (checkUserExistsH N E)) $
    header "Z-User"
      .&. capture "uid"
  document "HEAD" "userExists" $ do
    Doc.summary "Check if a user ID exists"
    Doc.parameter Doc.Path "uid" Doc.bytes' $
      Doc.description "User ID"
    Doc.response 200 "User exists" Doc.end
    Doc.errorResponse userNotFound
  ---

  -- if the user is expired, an internal event is handled asynchronously:
  --   UserDeleted event to contacts
  --   via galley: MemberLeave EdMembersLeave event to members for all conversations the user was in
  get "/users/:uid" (continue (getUserH N E)) $
    accept "application" "json"
      .&. header "Z-User"
      .&. capture "uid"
  document "GET" "user" $ do
    Doc.summary "Get a user by ID"
    Doc.parameter Doc.Path "uid" Doc.bytes' $
      Doc.description "User ID"
    Doc.returns (Doc.ref Doc.user)
    Doc.response 200 "User" Doc.end
    Doc.errorResponse userNotFound
  ---

  post "/users/handles" (continue checkHandlesH) $
    accept "application" "json"
      .&. header "Z-User"
      .&. jsonRequest @CheckHandles
  document "POST" "checkUserHandles" $ do
    Doc.summary "Check availability of user handles"
    Doc.body (Doc.ref Doc.checkHandles) $
      Doc.description "JSON body"
    Doc.returns (Doc.array Doc.string')
    Doc.response 200 "List of free handles" Doc.end
  head "/users/handles/:handle" (continue checkHandleH) $
    header "Z-User"
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
      .&. header "Z-User"
      .&. capture "handle"
  document "GET" "getUserHandleInfo" $ do
    Doc.summary "Get information on a user handle"
    Doc.parameter Doc.Path "handle" Doc.bytes' $
      Doc.description "The user handle"
    Doc.returns (Doc.ref Doc.userHandleInfo)
    Doc.response 200 "Handle info" Doc.end
    Doc.errorResponse handleNotFound
  ---

  -- if the user is expired, an internal event is handled asynchronously:
  --   UserDeleted event to contacts
  --   via galley: MemberLeave EdMembersLeave event to members for all conversations the user was in
  get "/users" (continue (listUsersH N E)) $
    accept "application" "json"
      .&. header "Z-User"
      .&. (param "ids" ||| param "handles")
  document "GET" "users" $ do
    Doc.summary "List users"
    Doc.notes "The 'ids' and 'handles' parameters are mutually exclusive."
    Doc.parameter Doc.Query "ids" Doc.string' $ do
      Doc.description "User IDs of users to fetch"
      Doc.optional
    Doc.parameter Doc.Query "handles" Doc.string' $ do
      Doc.description "Handles of users to fetch"
      Doc.optional
    Doc.returns (Doc.array (Doc.ref Doc.user))
    Doc.response 200 "List of users" Doc.end
  ---

  post "/users/prekeys" (continue getMultiPrekeyBundlesH) $
    jsonRequest @UserClients
      .&. accept "application" "json"
  document "POST" "getMultiPrekeyBundles" $ do
    Doc.summary
      "Given a map of user IDs to client IDs return a \
      \prekey for each one. You can't request information for more users than \
      \maximum conversation size."
    Doc.notes
      "Prekeys of all clients of a multiple users. \
      \The result is a map of maps, i.e. { UserId : { ClientId : Maybe Prekey } }"
    Doc.body (Doc.ref Doc.userClients) $
      Doc.description "JSON body"
    Doc.response 200 "Prekey Bundles" Doc.end
    Doc.errorResponse tooManyClients
  ---

  get "/users/:uid/prekeys" (continue getPrekeyBundleH) $
    capture "uid"
      .&. accept "application" "json"
  document "GET" "getPrekeyBundle" $ do
    Doc.summary "Get a prekey for each client of a user."
    Doc.parameter Doc.Path "uid" Doc.bytes' $
      Doc.description "User ID"
    Doc.returns (Doc.ref Doc.prekeyBundle)
    Doc.response 200 "Prekey Bundle" Doc.end
  ---

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
    Doc.returns (Doc.ref Doc.clientPrekey)
    Doc.response 200 "Client Prekey" Doc.end
  --

  get "/users/:uid/clients" (continue getUserClientsH) $
    capture "uid"
      .&. accept "application" "json"
  document "GET" "getUserClients" $ do
    Doc.summary "Get all of a user's clients."
    Doc.parameter Doc.Path "uid" Doc.bytes' $
      Doc.description "User ID"
    Doc.returns (Doc.array (Doc.ref Doc.pubClient))
    Doc.response 200 "List of clients" Doc.end
  --

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
    Doc.returns (Doc.ref Doc.pubClient)
    Doc.response 200 "Client" Doc.end
  --

  get "/users/:uid/rich-info" (continue getRichInfoH) $
    header "Z-User"
      .&. capture "uid"
      .&. accept "application" "json"
  document "GET" "getRichInfo" $ do
    Doc.summary "Get user's rich info"
    Doc.parameter Doc.Path "uid" Doc.bytes' $
      Doc.description "User ID"
    Doc.returns (Doc.ref Doc.richInfo)
    Doc.response 200 "RichInfo" Doc.end
    Doc.errorResponse insufficientTeamPermissions
  -- /self ------------------------------------------------------------------

  -- Profile

  get "/self" (continue getSelfH) $
    accept "application" "json"
      .&. header "Z-User"
  document "GET" "self" $ do
    Doc.summary "Get your profile"
    Doc.returns (Doc.ref Doc.self)
    Doc.response 200 "Self profile" Doc.end
  ---

  -- UserUpdated event to contacts
  put "/self" (continue (updateUserH E)) $
    header "Z-User"
      .&. header "Z-Connection"
      .&. jsonRequest @UserUpdate
  document "PUT" "updateSelf" $ do
    Doc.summary "Update your profile"
    Doc.body (Doc.ref Doc.userUpdate) $
      Doc.description "JSON body"
    Doc.response 200 "Update successful." Doc.end
  ---

  get "/self/name" (continue getUserNameH) $
    accept "application" "json"
      .&. header "Z-User"
  document "GET" "selfName" $ do
    Doc.summary "Get your profile name"
    Doc.returns (Doc.ref Doc.userName)
    Doc.response 200 "Profile name found." Doc.end
  ---

  put "/self/email" (continue changeSelfEmailH) $
    header "Z-User"
      .&. header "Z-Connection"
      .&. jsonRequest @EmailUpdate
  document "PUT" "changeEmail" $ do
    Doc.summary "Change your email address"
    Doc.body (Doc.ref Doc.emailUpdate) $
      Doc.description "JSON body"
    Doc.response 202 "Update accepted and pending activation of the new email." Doc.end
    Doc.response 204 "No update, current and new email address are the same." Doc.end
    Doc.errorResponse invalidEmail
    Doc.errorResponse userKeyExists
    Doc.errorResponse blacklistedEmail
    Doc.errorResponse blacklistedPhone
  ---

  put "/self/phone" (continue changePhoneH) $
    header "Z-User"
      .&. header "Z-Connection"
      .&. jsonRequest @PhoneUpdate
  document "PUT" "changePhone" $ do
    Doc.summary "Change your phone number"
    Doc.body (Doc.ref Doc.phoneUpdate) $
      Doc.description "JSON body"
    Doc.response 202 "Update accepted and pending activation of the new phone number." Doc.end
    Doc.errorResponse userKeyExists
  ---

  head "/self/password" (continue checkPasswordExistsH) $
    header "Z-User"
  document "HEAD" "checkPassword" $ do
    Doc.summary "Check that your password is set"
    Doc.response 200 "Password is set." Doc.end
    Doc.response 404 "Password is not set." Doc.end
  ---

  put "/self/password" (continue changePasswordH) $
    header "Z-User"
      .&. jsonRequest @PasswordChange
  document "PUT" "changePassword" $ do
    Doc.summary "Change your password"
    Doc.body (Doc.ref Doc.changePassword) $
      Doc.description "JSON body"
    Doc.response 200 "Password changed." Doc.end
    Doc.errorResponse badCredentials
    Doc.errorResponse noIdentity
  --

  put "/self/locale" (continue changeLocaleH) $
    header "Z-User"
      .&. header "Z-Connection"
      .&. jsonRequest @LocaleUpdate
  document "PUT" "changeLocale" $ do
    Doc.summary "Change your locale"
    Doc.body (Doc.ref Doc.changeLocale) $
      Doc.description "JSON body"
    Doc.response 200 "Locale changed." Doc.end
  --

  -- UserUpdated event to contacts
  put "/self/handle" (continue (changeHandleH E)) $
    header "Z-User"
      .&. header "Z-Connection"
      .&. jsonRequest @HandleUpdate
  document "PUT" "changeHandle" $ do
    Doc.summary "Change your handle"
    Doc.body (Doc.ref Doc.changeHandle) $
      Doc.description "JSON body"
    Doc.errorResponse handleExists
    Doc.errorResponse invalidHandle
    Doc.response 200 "Handle changed." Doc.end
  ---

  -- UserIdentityRemoved event to self
  delete "/self/phone" (continue (removePhoneH E)) $
    header "Z-User"
      .&. header "Z-Connection"
  document "DELETE" "removePhone" $ do
    Doc.summary "Remove your phone number."
    Doc.notes
      "Your phone number can only be removed if you also have an \
      \email address and a password."
    Doc.response 200 "Phone number removed." Doc.end
    Doc.errorResponse lastIdentity
    Doc.errorResponse noPassword
  ---

  -- UserIdentityRemoved event to self
  delete "/self/email" (continue (removeEmailH E)) $
    header "Z-User"
      .&. header "Z-Connection"
  document "DELETE" "removeEmail" $ do
    Doc.summary "Remove your email address."
    Doc.notes
      "Your email address can only be removed if you also have a \
      \phone number."
    Doc.response 200 "Email address removed." Doc.end
    Doc.errorResponse lastIdentity
  ---
  ---

  -- UserDeleted event to contacts
  -- via galley: MemberLeave EdMembersLeave event to members for all conversations the user was in
  delete "/self" (continue (deleteUserH N E)) $
    header "Z-User"
      .&. jsonRequest @DeleteUser
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
    Doc.body (Doc.ref Doc.delete) $
      Doc.description "JSON body"
    Doc.response 202 "Deletion is pending verification with a code." Doc.end
    Doc.response 200 "Deletion is initiated." Doc.end
    Doc.errorResponse badCredentials
    Doc.errorResponse missingAuthError
  ---

  -- UserDeleted event to contacts
  -- via galley: MemberLeave EdMembersLeave event to members for all conversations the user was in
  post "/delete" (continue (verifyDeleteUserH N E)) $
    jsonRequest @VerifyDeleteUser
      .&. accept "application" "json"
  document "POST" "verifyDeleteUser" $ do
    Doc.summary "Verify account deletion with a code."
    Doc.body (Doc.ref Doc.verifyDelete) $
      Doc.description "JSON body"
    Doc.response 200 "Deletion is initiated." Doc.end
    Doc.errorResponse invalidCode
  ---

  -- ConnectionUpdated event to self and other, unless both states already exist and (1) any state is blocked or (2) both sides already accepted
  --
  -- potentially can cause events to be sent from Galley:
  -- if only the other already was member of connect conversation and has connection status Sent or Accepted:
  --   via galley: MemberJoin EdMembersJoin event to you and other
  --
  --  Also possible (for details check 'Galley.API.Create.createConnectConversation'):
  --   via galley: ConvCreate EdConversation event to self
  --   via galley: ConvConnect EdConnect event to self
  post "/connections" (continue (createConnectionH N E)) $
    accept "application" "json"
      .&. header "Z-User"
      .&. header "Z-Connection"
      .&. jsonRequest @ConnectionRequest
  document "POST" "createConnection" $ do
    Doc.summary "Create a connection to another user."
    Doc.notes $
      "You can have no more than "
        <> Text.pack (show (setUserMaxConnections $ optSettings o))
        <> " connections in accepted or sent state."
    Doc.body (Doc.ref Doc.connectionRequest) $
      Doc.description "JSON body"
    Doc.returns (Doc.ref Doc.connection)
    Doc.response 200 "The connection exists." Doc.end
    Doc.response 201 "The connection was created." Doc.end
    Doc.errorResponse connectionLimitReached
    Doc.errorResponse invalidUser
    Doc.errorResponse noIdentity
  ---

  get "/connections" (continue listConnectionsH) $
    accept "application" "json"
      .&. header "Z-User"
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
    Doc.returns (Doc.ref Doc.connectionList)
    Doc.response 200 "List of connections" Doc.end
  ---

  -- ConnectionUpdated event to self, if our state changes
  -- ConnectionUpdated event to other, if their state changes as well
  --
  -- when moving to Sent or Accepted, this potentially can cause events to be sent from Galley when joining the connect conversation:
  --   via galley: MemberJoin EdMembersJoin event to you
  --   via galley: MemberJoin EdMembersJoin event to other
  put "/connections/:id" (continue (updateConnectionH N E)) $
    accept "application" "json"
      .&. header "Z-User"
      .&. header "Z-Connection"
      .&. capture "id"
      .&. jsonRequest @ConnectionUpdate
  document "PUT" "updateConnection" $ do
    Doc.summary "Update a connection."
    Doc.parameter Doc.Path "id" Doc.bytes' $
      Doc.description "User ID"
    Doc.body (Doc.ref Doc.connectionUpdate) $
      Doc.description "JSON body"
    Doc.returns (Doc.ref Doc.connection)
    Doc.response 200 "Connection updated." Doc.end
    Doc.response 204 "No change." Doc.end
    Doc.errorResponse connectionLimitReached
    Doc.errorResponse invalidTransition
    Doc.errorResponse notConnected
    Doc.errorResponse invalidUser
  ---

  get "/connections/:id" (continue getConnectionH) $
    accept "application" "json"
      .&. header "Z-User"
      .&. capture "id"
  document "GET" "connection" $ do
    Doc.summary "Get an existing connection to another user."
    Doc.parameter Doc.Path "id" Doc.bytes' $
      Doc.description "User ID"
    Doc.returns (Doc.ref Doc.connection)
    Doc.response 200 "Connection" Doc.end
  --- Clients

  -- ClientAdded event to self
  -- ClientRemoved event to self, if removing old clients due to max number
  post "/clients" (continue (addClientH E)) $
    jsonRequest @NewClient
      .&. header "Z-User"
      .&. header "Z-Connection"
      .&. opt (header "X-Forwarded-For")
      .&. accept "application" "json"
  document "POST" "registerClient" $ do
    Doc.summary "Register a new client."
    Doc.body (Doc.ref Doc.newClient) $
      Doc.description "JSON body"
    Doc.returns (Doc.ref Doc.client)
    Doc.response 200 "Client" Doc.end
    Doc.errorResponse tooManyClients
    Doc.errorResponse missingAuthError
    Doc.errorResponse malformedPrekeys
  ---

  put "/clients/:client" (continue updateClientH) $
    jsonRequest @UpdateClient
      .&. header "Z-User"
      .&. capture "client"
      .&. accept "application" "json"
  document "PUT" "updateClient" $ do
    Doc.summary "Update a registered client."
    Doc.parameter Doc.Path "client" Doc.bytes' $
      Doc.description "Client ID"
    Doc.body (Doc.ref Doc.updateClient) $
      Doc.description "JSON body"
    Doc.response 200 "Client updated." Doc.end
    Doc.errorResponse malformedPrekeys
  ---

  -- ClientRemoved event to self
  delete "/clients/:client" (continue (rmClientH E)) $
    jsonRequest @RmClient
      .&. header "Z-User"
      .&. header "Z-Connection"
      .&. capture "client"
      .&. accept "application" "json"
  document "DELETE" "deleteClient" $ do
    Doc.summary "Delete an existing client."
    Doc.parameter Doc.Path "client" Doc.bytes' $
      Doc.description "Client ID"
    Doc.body (Doc.ref Doc.deleteClient) $
      Doc.description "JSON body"
    Doc.response 200 "Client deleted." Doc.end
  ---

  get "/clients" (continue listClientsH) $
    header "Z-User"
      .&. accept "application" "json"
  document "GET" "listClients" $ do
    Doc.summary "List the registered clients."
    Doc.returns (Doc.array (Doc.ref Doc.client))
    Doc.response 200 "List of clients" Doc.end
  ---

  get "/clients/:client" (continue getClientH) $
    header "Z-User"
      .&. capture "client"
      .&. accept "application" "json"
  document "GET" "getClients" $ do
    Doc.summary "Get a registered client by ID."
    Doc.parameter Doc.Path "client" Doc.bytes' $
      Doc.description "Client ID"
    Doc.returns (Doc.ref Doc.client)
    Doc.response 200 "Client" Doc.end
  ---

  get "/clients/:client/prekeys" (continue listPrekeyIdsH) $
    header "Z-User"
      .&. capture "client"
      .&. accept "application" "json"
  document "GET" "listPrekeyIds" $ do
    Doc.summary "List the remaining prekey IDs of a client."
    Doc.parameter Doc.Path "client" Doc.bytes' $
      Doc.description "Client ID"
    Doc.returns (Doc.array Doc.string')
    Doc.response 200 "List of remaining prekey IDs." Doc.end
  --- Properties

  -- PropertySet event to self
  put "/properties/:key" (continue (setPropertyH E)) $
    header "Z-User"
      .&. header "Z-Connection"
      .&. capture "key"
      .&. jsonRequest @PropertyValue
  document "PUT" "setProperty" $ do
    Doc.summary "Set a user property."
    Doc.parameter Doc.Path "key" Doc.string' $
      Doc.description "Property key"
    Doc.body (Doc.ref Doc.propertyValue) $
      Doc.description "JSON body"
    Doc.response 200 "Property set." Doc.end
  ---

  -- PropertyDeleted event to self
  delete "/properties/:key" (continue (deletePropertyH E)) $
    header "Z-User"
      .&. header "Z-Connection"
      .&. capture "key"
  document "DELETE" "deleteProperty" $ do
    Doc.summary "Delete a property."
    Doc.parameter Doc.Path "key" Doc.string' $
      Doc.description "Property key"
    Doc.response 200 "Property deleted." Doc.end
  ---

  -- PropertiesCleared event to self
  delete "/properties" (continue (clearPropertiesH E)) $
    header "Z-User"
      .&. header "Z-Connection"
  document "DELETE" "clearProperties" $ do
    Doc.summary "Clear all properties."
    Doc.response 200 "Properties cleared." Doc.end
  ---

  get "/properties/:key" (continue getPropertyH) $
    header "Z-User"
      .&. capture "key"
      .&. accept "application" "json"
  document "GET" "getProperty" $ do
    Doc.summary "Get a property value."
    Doc.parameter Doc.Path "key" Doc.string' $
      Doc.description "Property key"
    Doc.returns (Doc.ref Doc.propertyValue)
    Doc.response 200 "The property value." Doc.end
  ---

  get "/properties" (continue listPropertyKeysH) $
    header "Z-User"
      .&. accept "application" "json"
  document "GET" "listPropertyKeys" $ do
    Doc.summary "List all property keys."
    Doc.returns (Doc.array Doc.string')
    Doc.response 200 "List of property keys." Doc.end
  ---

  get "/properties-values" (continue listPropertyKeysAndValuesH) $
    header "Z-User"
      .&. accept "application" "json"
  document "GET" "listPropertyKeysAndValues" $ do
    Doc.summary "List all properties with key and value."
    Doc.returns (Doc.ref Doc.propertyDictionary)
    Doc.response 200 "Object with properties as attributes." Doc.end
  -- /register, /activate, /password-reset ----------------------------------

  -- docs/reference/user/registration.md {#RefRegistration}
  --
  -- UserActivated event to self, if user is team user
  -- UserActivated event to self, if user has SSO ID
  -- UserIdentityUpdated event to self, if user has newUserPhoneCode
  -- UserIdentityUpdated event to self, if user has newUserEmailCode (unless team email invite)
  post "/register" (continue (createUserH E)) $
    accept "application" "json"
      .&. jsonRequest @NewUserPublic
  document "POST" "register" $ do
    Doc.summary "Register a new user."
    Doc.notes
      "If the environment where the registration takes \
      \place is private and a registered email address or phone \
      \number is not whitelisted, a 403 error is returned."
    Doc.body (Doc.ref Doc.newUser) $
      Doc.description "JSON body"
    Doc.returns (Doc.ref Doc.user)
    Doc.response 201 "User created and pending activation." Doc.end
    Doc.errorResponse whitelistError
    Doc.errorResponse invalidInvitationCode
    Doc.errorResponse missingIdentity
    Doc.errorResponse userKeyExists
    Doc.errorResponse activationCodeNotFound
    Doc.errorResponse blacklistedEmail
    Doc.errorResponse blacklistedPhone
  ---

  -- UserActivated event to self, if account gets activated
  -- UserIdentityUpdated event to self, if email or phone get activated
  get "/activate" (continue (activateH E)) $
    query "key"
      .&. query "code"
  document "GET" "activate" $ do
    Doc.summary "Activate (i.e. confirm) an email address or phone number."
    Doc.notes "See also 'POST /activate' which has a larger feature set."
    Doc.parameter Doc.Query "key" Doc.bytes' $
      Doc.description "Activation key"
    Doc.parameter Doc.Query "code" Doc.bytes' $
      Doc.description "Activation code"
    Doc.returns (Doc.ref Doc.activationResponse)
    Doc.response 200 "Activation successful." Doc.end
    Doc.response 204 "A recent activation was already successful." Doc.end
    Doc.errorResponse activationCodeNotFound
  ---

  -- docs/reference/user/activation.md {#RefActivationSubmit}
  --
  -- UserActivated event to self, if account gets activated
  -- UserIdentityUpdated event to self, if email or phone get activated
  post "/activate" (continue (activateKeyH E)) $
    accept "application" "json"
      .&. jsonRequest @Activate
  document "POST" "activate" $ do
    Doc.summary "Activate (i.e. confirm) an email address or phone number."
    Doc.notes
      "Activation only succeeds once and the number of \
      \failed attempts for a valid key is limited."
    Doc.body (Doc.ref Doc.activate) $
      Doc.description "JSON body"
    Doc.returns (Doc.ref Doc.activationResponse)
    Doc.response 200 "Activation successful." Doc.end
    Doc.response 204 "A recent activation was already successful." Doc.end
    Doc.errorResponse activationCodeNotFound
  ---

  -- docs/reference/user/activation.md {#RefActivationRequest}
  post "/activate/send" (continue sendActivationCodeH) $
    jsonRequest @SendActivationCode
  document "POST" "sendActivationCode" $ do
    Doc.summary "Send (or resend) an email or phone activation code."
    Doc.body (Doc.ref Doc.sendActivationCode) $
      Doc.description "JSON body"
    Doc.response 200 "Activation code sent." Doc.end
    Doc.errorResponse invalidEmail
    Doc.errorResponse invalidPhone
    Doc.errorResponse userKeyExists
    Doc.errorResponse blacklistedEmail
    Doc.errorResponse blacklistedPhone
  ---

  post "/password-reset" (continue beginPasswordResetH) $
    accept "application" "json"
      .&. jsonRequest @NewPasswordReset
  document "POST" "beginPasswordReset" $ do
    Doc.summary "Initiate a password reset."
    Doc.body (Doc.ref Doc.newPasswordReset) $
      Doc.description "JSON body"
    Doc.response 201 "Password reset code created and sent by email." Doc.end
    Doc.errorResponse invalidPwResetKey
    Doc.errorResponse duplicatePwResetCode
  ---

  post "/password-reset/complete" (continue completePasswordResetH) $
    accept "application" "json"
      .&. jsonRequest @CompletePasswordReset
  document "POST" "completePasswordReset" $ do
    Doc.summary "Complete a password reset."
    Doc.body (Doc.ref Doc.completePasswordReset) $
      Doc.description "JSON body"
    Doc.response 200 "Password reset successful." Doc.end
    Doc.errorResponse invalidPwResetCode
  ---

  post "/password-reset/:key" (continue deprecatedCompletePasswordResetH) $
    accept "application" "json"
      .&. capture "key"
      .&. jsonRequest @PasswordReset
  document "POST" "deprecatedCompletePasswordReset" $ do
    Doc.deprecated
    Doc.summary "Complete a password reset."
    Doc.notes "DEPRECATED: Use 'POST /password-reset/complete'."
  ---

  post "/onboarding/v3" (continue deprecatedOnboardingH) $
    accept "application" "json"
      .&. header "Z-User"
      .&. jsonRequest @Value
  document "POST" "onboardingV3" $ do
    Doc.deprecated
    Doc.summary "Upload contacts and invoke matching."
    Doc.notes
      "DEPRECATED: the feature has been turned off, the end-point does \
      \nothing and always returns '{\"results\":[],\"auto-connects\":[]}'."
  -----

  Provider.routes
  Auth.routes
  Search.routes
  Team.routes
  TURN.routes

---------------------------------------------------------------------------
-- Handlers

-- PropertySet event to self
setPropertyH :: E -> UserId ::: ConnId ::: PropertyKey ::: JsonRequest PropertyValue -> Handler Response
setPropertyH E (u ::: c ::: k ::: req) = do
  propkey <- safeParsePropertyKey k
  propval <- safeParsePropertyValue (lazyRequestBody (fromJsonRequest req))
  -- PropertySet event to self
  empty <$ setProperty E u c propkey propval

-- PropertySet event to self
setProperty :: E -> UserId -> ConnId -> PropertyKey -> PropertyValue -> Handler ()
setProperty E u c propkey propval = do
  -- PropertySet event to self
  API.setProperty E u c propkey propval !>> propDataError

safeParsePropertyKey :: PropertyKey -> Handler PropertyKey
safeParsePropertyKey k = do
  maxKeyLen <- fromMaybe defMaxKeyLen <$> view (settings . propertyMaxKeyLen)
  unless (Text.compareLength (Ascii.toText (propertyKeyName k)) (fromIntegral maxKeyLen) <= EQ) $
    throwStd propertyKeyTooLarge
  pure k

-- | Parse a 'PropertyValue' from a bytestring.  This is different from 'FromJSON' in that
-- checks the byte size of the input, and fails *without consuming all of it* if that size
-- exceeds the settings.
safeParsePropertyValue :: IO Lazy.ByteString -> Handler PropertyValue
safeParsePropertyValue lreqbody = do
  maxValueLen <- fromMaybe defMaxValueLen <$> view (settings . propertyMaxValueLen)
  lbs <- Lazy.take (maxValueLen + 1) <$> liftIO lreqbody
  unless (Lazy.length lbs <= maxValueLen) $
    throwStd propertyValueTooLarge
  hoistEither $ fmapL (StdError . badRequest . pack) (eitherDecode lbs)

-- PropertyDeleted event to self
deletePropertyH :: E -> UserId ::: ConnId ::: PropertyKey -> Handler Response
deletePropertyH E (u ::: c ::: k) = lift (API.deleteProperty E u c k) >> return empty

-- PropertiesCleared event to self
clearPropertiesH :: E -> UserId ::: ConnId -> Handler Response
clearPropertiesH E (u ::: c) = lift (API.clearProperties E u c) >> return empty

getPropertyH :: UserId ::: PropertyKey ::: JSON -> Handler Response
getPropertyH (u ::: k ::: _) = do
  val <- lift $ API.lookupProperty u k
  return $ case val of
    Nothing -> setStatus status404 empty
    Just v -> json v

listPropertyKeysH :: UserId ::: JSON -> Handler Response
listPropertyKeysH (u ::: _) = json <$> lift (API.lookupPropertyKeys u)

listPropertyKeysAndValuesH :: UserId ::: JSON -> Handler Response
listPropertyKeysAndValuesH (u ::: _) = json <$> lift (API.lookupPropertyKeysAndValues u)

getPrekeyH :: OpaqueUserId ::: ClientId ::: JSON -> Handler Response
getPrekeyH (u ::: c ::: _) = do
  getPrekey u c <&> \case
    Just pk -> json pk
    Nothing -> setStatus status404 empty

getPrekey :: OpaqueUserId -> ClientId -> Handler (Maybe ClientPrekey)
getPrekey u c = do
  resolvedUserId <- resolveOpaqueUserId u
  lift $ API.claimPrekey resolvedUserId c

getPrekeyBundleH :: OpaqueUserId ::: JSON -> Handler Response
getPrekeyBundleH (u ::: _) = json <$> getPrekeyBundle u

getPrekeyBundle :: OpaqueUserId -> Handler PrekeyBundle
getPrekeyBundle u = do
  resolvedUserId <- resolveOpaqueUserId u
  lift $ API.claimPrekeyBundle resolvedUserId

getMultiPrekeyBundlesH :: JsonRequest UserClients ::: JSON -> Handler Response
getMultiPrekeyBundlesH (req ::: _) = do
  json <$> (getMultiPrekeyBundles =<< parseJsonBody req)

getMultiPrekeyBundles :: UserClients -> Handler (UserClientMap (Maybe Prekey))
getMultiPrekeyBundles body = do
  maxSize <- fromIntegral . setMaxConvSize <$> view settings
  when (Map.size (userClients body) > maxSize) $
    throwStd tooManyClients
  API.claimMultiPrekeyBundles body

-- ClientAdded event to self
-- ClientRemoved event to self, if removing old clients due to max number
addClientH :: E -> JsonRequest NewClient ::: UserId ::: ConnId ::: Maybe IpAddr ::: JSON -> Handler Response
addClientH E (req ::: usr ::: con ::: ip ::: _) = do
  new <- parseJsonBody req
  -- ClientAdded event to self
  -- ClientRemoved event to self, if removing old clients due to max number
  clt <- addClient E new usr con ip
  let loc = toByteString' $ clientId clt
  pure . setStatus status201 . addHeader "Location" loc . json $ clt

-- ClientAdded event to self
-- ClientRemoved event to self, if removing old clients due to max number
addClient :: E -> NewClient -> UserId -> ConnId -> Maybe IpAddr -> Handler Client
addClient E new usr con ip = do
  -- Users can't add legal hold clients
  when (newClientType new == LegalHoldClientType) $
    throwE (clientError ClientLegalHoldCannotBeAdded)
  -- ClientAdded event to self
  -- UserLegalHoldEnabled event to contacts, if client is legalhold
  -- ClientRemoved event to self, if removing old clients
  API.addClient E usr (Just con) (ipAddr <$> ip) new !>> clientError

-- | Add a client without authentication checks
--
-- ClientAdded event to self
-- UserLegalHoldEnabled event to contacts, if client is legalhold
-- ClientRemoved event to self, if removing old clients
addClientInternalH :: E -> UserId ::: JsonRequest NewClient ::: Maybe ConnId ::: JSON -> Handler Response
addClientInternalH E (usr ::: req ::: connId ::: _) = do
  new <- parseJsonBody req
  setStatus status201 . json <$> addClientInternal E usr new connId

-- ClientAdded event to self
-- UserLegalHoldEnabled event to contacts, if client is legalhold
-- ClientRemoved event to self, if removing old clients
addClientInternal :: E -> UserId -> NewClient -> Maybe ConnId -> Handler Client
addClientInternal E usr new connId = do
  API.addClient E usr connId Nothing new !>> clientError

-- ClientRemoved event to self
rmClientH :: E -> JsonRequest RmClient ::: UserId ::: ConnId ::: ClientId ::: JSON -> Handler Response
rmClientH E (req ::: usr ::: con ::: clt ::: _) = do
  body <- parseJsonBody req
  empty <$ rmClient E body usr con clt

-- ClientRemoved event to self
rmClient :: E -> RmClient -> UserId -> ConnId -> ClientId -> Handler ()
rmClient E body usr con clt = do
  -- ClientRemoved event to self
  API.rmClient E usr con clt (rmPassword body) !>> clientError

-- LegalHoldClientRequested event to contacts
legalHoldClientRequestedH :: E -> UserId ::: JsonRequest LegalHoldClientRequest ::: JSON -> Handler Response
legalHoldClientRequestedH E (targetUser ::: req ::: _) = do
  clientRequest <- parseJsonBody req
  -- LegalHoldClientRequested event to contacts
  lift $ API.legalHoldClientRequested E targetUser clientRequest
  return $ setStatus status200 empty

-- ClientRemoved event to self
-- UserLegalHoldDisabled event to contacts
removeLegalHoldClientH :: E -> UserId ::: JSON -> Handler Response
removeLegalHoldClientH E (uid ::: _) = do
  lift $ API.removeLegalHoldClient E uid
  return $ setStatus status200 empty

updateClientH :: JsonRequest UpdateClient ::: UserId ::: ClientId ::: JSON -> Handler Response
updateClientH (req ::: usr ::: clt ::: _) = do
  body <- parseJsonBody req
  empty <$ updateClient body usr clt

updateClient :: UpdateClient -> UserId -> ClientId -> Handler ()
updateClient body usr clt = do
  API.updateClient usr clt body !>> clientError

listClientsH :: OpaqueUserId ::: JSON -> Handler Response
listClientsH (opaqueUserId ::: _) =
  json <$> listClients opaqueUserId

listClients :: OpaqueUserId -> Handler [Client]
listClients opaqueUserId = do
  resolvedUserId <- resolveOpaqueUserId opaqueUserId
  API.lookupClients resolvedUserId !>> clientError

internalListClientsH :: JSON ::: JsonRequest UserSet -> Handler Response
internalListClientsH (_ ::: req) = do
  json <$> (lift . internalListClients =<< parseJsonBody req)

internalListClients :: UserSet -> AppIO UserClients
internalListClients (UserSet usrs) = do
  UserClients . Map.mapKeys makeIdOpaque . Map.fromList
    <$> (API.lookupUsersClientIds $ Set.toList usrs)

getClientH :: OpaqueUserId ::: ClientId ::: JSON -> Handler Response
getClientH (usr ::: clt ::: _) =
  getClient usr clt <&> \case
    Just c -> json c
    Nothing -> setStatus status404 empty

getClient :: OpaqueUserId -> ClientId -> Handler (Maybe Client)
getClient opaqueUserId clientId = do
  resolvedUserId <- resolveOpaqueUserId opaqueUserId
  API.lookupClient resolvedUserId clientId !>> clientError

getUserClientsH :: OpaqueUserId ::: JSON -> Handler Response
getUserClientsH (user ::: _) =
  json <$> getUserClients user

getUserClients :: OpaqueUserId -> Handler [PubClient]
getUserClients opaqueUserId = do
  resolvedUserId <- resolveOpaqueUserId opaqueUserId
  API.pubClient <$$> API.lookupClients resolvedUserId !>> clientError

getUserClientH :: OpaqueUserId ::: ClientId ::: JSON -> Handler Response
getUserClientH (user ::: cid ::: _) = do
  maybe (setStatus status404 empty) json <$> getUserClient user cid

getUserClient :: OpaqueUserId -> ClientId -> Handler (Maybe PubClient)
getUserClient opaqueUserId clientId = do
  resolvedUserId <- resolveOpaqueUserId opaqueUserId
  API.pubClient <$$> API.lookupClient resolvedUserId clientId !>> clientError

getRichInfoH :: UserId ::: UserId ::: JSON -> Handler Response
getRichInfoH (self ::: user ::: _) = do
  json <$> getRichInfo self user

getRichInfo :: UserId -> UserId -> Handler RichInfoAssocList
getRichInfo self user = do
  -- Check that both users exist and the requesting user is allowed to see rich info of the
  -- other user
  selfUser <- ifNothing userNotFound =<< lift (Data.lookupUser self)
  otherUser <- ifNothing userNotFound =<< lift (Data.lookupUser user)
  case (userTeam selfUser, userTeam otherUser) of
    (Just t1, Just t2) | t1 == t2 -> pure ()
    _ -> throwStd insufficientTeamPermissions
  -- Query rich info
  fromMaybe emptyRichInfoAssocList <$> lift (API.lookupRichInfo user)

listPrekeyIdsH :: UserId ::: ClientId ::: JSON -> Handler Response
listPrekeyIdsH (usr ::: clt ::: _) = json <$> lift (API.lookupPrekeyIds usr clt)

-- ConnectionUpdated event to self and users connecting with
--
-- This will cause events to be sent from Galley:
-- for connect conversations that did not exist before:
--   via galley: ConvCreate EdConversation event to self
-- if others didn't join a connect conversation with self before:
--   via galley: ConvConnect EdConnect event to self
--   via galley: MemberJoin EdMembersJoin event to you and other
autoConnectH :: N -> E -> JSON ::: UserId ::: Maybe ConnId ::: JsonRequest UserSet -> Handler Response
autoConnectH N E (_ ::: uid ::: conn ::: req) = do
  json <$> (autoConnect N E uid conn =<< parseJsonBody req)

-- ConnectionUpdated event to self and users connecting with
--
-- This will cause events to be sent from Galley:
-- for connect conversations that did not exist before:
--   via galley: ConvCreate EdConversation event to self
-- if others didn't join a connect conversation with self before:
--   via galley: ConvConnect EdConnect event to self
--   via galley: MemberJoin EdMembersJoin event to you and other
autoConnect :: N -> E -> UserId -> Maybe ConnId -> UserSet -> Handler [UserConnection]
autoConnect N E uid conn (UserSet to) = do
  let num = Set.size to
  when (num < 1)
    $ throwStd
    $ badRequest "No users given for auto-connect."
  when (num > 25)
    $ throwStd
    $ badRequest "Too many users given for auto-connect (> 25)."
  -- ConnectionUpdated event to self and users connecting with
  --
  -- This will cause events to be sent from Galley:
  -- for connect conversations that did not exist before:
  --   via galley: ConvCreate EdConversation event to self
  -- if others didn't join a connect conversation with self before:
  --   via galley: ConvConnect EdConnect event to self
  --   via galley: MemberJoin EdMembersJoin event to you and other
  API.autoConnect N E uid to conn !>> connError

-- docs/reference/user/registration.md {#RefRegistration}
--
-- UserActivated event to self, if user is team user
-- UserActivated event to self, if user has SSO ID
-- UserIdentityUpdated event to self, if user has newUserPhoneCode
-- UserIdentityUpdated event to self, if user has newUserEmailCode (unless team email invite)
createUserH :: E -> JSON ::: JsonRequest NewUserPublic -> Handler Response
createUserH E (_ ::: req) = do
  CreateUserResponse cok loc prof <- createUser E =<< parseJsonBody req
  lift . Auth.setResponseCookie cok
    . setStatus status201
    . addHeader "Location" (toByteString' loc)
    $ json prof

data CreateUserResponse = CreateUserResponse (Cookie (ZAuth.Token ZAuth.User)) UserId SelfProfile

-- UserActivated event to self, if user is team user
-- UserActivated event to self, if user has SSO ID
-- UserIdentityUpdated event to self, if user has newUserPhoneCode
-- UserIdentityUpdated event to self, if user has newUserEmailCode (unless team email invite)
createUser :: E -> NewUserPublic -> Handler CreateUserResponse
createUser E (NewUserPublic new) = do
  for_ (newUserEmail new) $ checkWhitelist . Left
  for_ (newUserPhone new) $ checkWhitelist . Right
  -- UserActivated event to self, if user is team user
  -- UserActivated event to self, if user has SSO ID
  -- UserIdentityUpdated event to self, if user has newUserPhoneCode
  -- UserIdentityUpdated event to self, if user has newUserEmailCode (unless team email invite)
  result <- API.createUser E new !>> newUserError
  let acc = createdAccount result
  let usr = accountUser acc
  let eac = createdEmailActivation result
  let pac = createdPhoneActivation result
  let epair = (,) <$> (activationKey <$> eac) <*> (activationCode <$> eac)
  let ppair = (,) <$> (activationKey <$> pac) <*> (activationCode <$> pac)
  let lang = userLocale usr
  lift $ do
    for_ (liftM2 (,) (userEmail usr) epair) $ \(e, p) ->
      sendActivationEmail e (userName usr) p (Just lang) (newUserTeam new)
    for_ (liftM2 (,) (userPhone usr) ppair) $ \(p, c) ->
      sendActivationSms p c (Just lang)
    for_ (liftM3 (,,) (userEmail usr) (createdUserTeam result) (newUserTeam new)) $ \(e, ct, ut) ->
      sendWelcomeEmail e ct ut (Just lang)
  cok <- case acc of
    UserAccount _ Ephemeral -> lift $ Auth.newCookie @ZAuth.User (userId usr) SessionCookie (newUserLabel new)
    UserAccount _ _ -> lift $ Auth.newCookie @ZAuth.User (userId usr) PersistentCookie (newUserLabel new)
  pure $ CreateUserResponse cok (userId usr) (SelfProfile usr)
  where
    sendActivationEmail e u p l (Just (NewTeamCreator (BindingNewTeamUser (Team.BindingNewTeam t) _))) =
      sendTeamActivationMail e u p l (fromRange $ t ^. Team.newTeamName)
    sendActivationEmail e u p l _ =
      sendActivationMail e u p l Nothing
    sendWelcomeEmail :: Email -> CreateUserTeam -> NewTeamUser -> Maybe Locale -> AppIO ()
    -- NOTE: Welcome e-mails for the team creator are not dealt by brig anymore
    sendWelcomeEmail _ (CreateUserTeam _ _) (NewTeamCreator _) _ = return ()
    sendWelcomeEmail e (CreateUserTeam t n) (NewTeamMember _) l = Team.sendMemberWelcomeMail e t n l
    sendWelcomeEmail e (CreateUserTeam t n) (NewTeamMemberSSO _) l = Team.sendMemberWelcomeMail e t n l

-- UserActivated event to self, if user is team user
-- UserActivated event to self, if user has SSO ID
-- UserIdentityUpdated event to self, if email or phone get activated (should always happen)
createUserNoVerifyH :: E -> JSON ::: JsonRequest NewUser -> Handler Response
createUserNoVerifyH E (_ ::: req) = do
  CreateUserNoVerifyResponse uid prof <- createUserNoVerify E =<< parseJsonBody req
  return . setStatus status201
    . addHeader "Location" (toByteString' uid)
    $ json prof

data CreateUserNoVerifyResponse = CreateUserNoVerifyResponse UserId SelfProfile

-- UserActivated event to self, if user is team user
-- UserActivated event to self, if user has SSO ID
-- UserIdentityUpdated event to self, if email or phone get activated (should always happen)
createUserNoVerify :: E -> NewUser -> Handler CreateUserNoVerifyResponse
createUserNoVerify E uData = do
  -- UserActivated event to self, if user is team user
  -- UserActivated event to self, if user has SSO ID
  -- UserIdentityUpdated event to self, if user has newUserPhoneCode
  -- UserIdentityUpdated event to self, if user has newUserEmailCode (unless team email invite)
  result <- API.createUser E uData !>> newUserError
  let acc = createdAccount result
  let usr = accountUser acc
  let uid = userId usr
  let eac = createdEmailActivation result
  let pac = createdPhoneActivation result
  for_ (catMaybes [eac, pac]) $ \adata ->
    let key = ActivateKey $ activationKey adata
        code = activationCode adata
     in -- UserActivated event to self, if account gets activated
        -- (not happening)
        -- UserIdentityUpdated event to self, if email or phone get activated
        -- (if eac or pac are set, which is if they were not already activated in createUser)
        API.activate E key code (Just uid) !>> actError
  return $ CreateUserNoVerifyResponse uid (SelfProfile usr)

-- when internal event is handled asynchronously:
--   UserDeleted event to contacts
--   via galley: MemberLeave EdMembersLeave event to members for all conversations the user was in
deleteUserNoVerifyH :: N -> E -> UserId -> Handler Response
deleteUserNoVerifyH N E uid = do
  setStatus status202 empty <$ deleteUserNoVerify N E uid

-- when internal event is handled asynchronously:
--   UserDeleted event to contacts
--   via galley: MemberLeave EdMembersLeave event to members for all conversations the user was in
deleteUserNoVerify :: N -> E -> UserId -> Handler ()
deleteUserNoVerify N E uid = do
  void $ lift (API.lookupAccount uid) >>= ifNothing userNotFound
  -- when internal event is handled asynchronously:
  --   UserDeleted event to contacts
  --   via galley: MemberLeave EdMembersLeave event to members for all conversations the user was in
  lift $ API.deleteUserNoVerify N E uid

changeSelfEmailNoSendH :: UserId ::: JsonRequest EmailUpdate -> Handler Response
changeSelfEmailNoSendH (u ::: req) = changeEmail u req False

-- if the user is expired, an internal event is handled asynchronously:
--   UserDeleted event to contacts
--   via galley: MemberLeave EdMembersLeave event to members for all conversations the user was in
checkUserExistsH :: N -> E -> UserId ::: OpaqueUserId -> Handler Response
checkUserExistsH N E (self ::: uid) = do
  exists <- checkUserExists N E self uid
  if exists then return empty else throwStd userNotFound

-- if the user is expired, an internal event is handled asynchronously:
--   UserDeleted event to contacts
--   via galley: MemberLeave EdMembersLeave event to members for all conversations the user was in
checkUserExists :: N -> E -> UserId -> OpaqueUserId -> Handler Bool
checkUserExists N E self opaqueUserId = do
  -- when internal event is handled asynchronously:
  --   UserDeleted event to contacts
  --   via galley: MemberLeave EdMembersLeave event to members for all conversations the user was in
  isJust <$> getUser N E self opaqueUserId

getSelfH :: JSON ::: UserId -> Handler Response
getSelfH (_ ::: self) = do
  json <$> getSelf self

getSelf :: UserId -> Handler SelfProfile
getSelf self = do
  lift (API.lookupSelfProfile self) >>= ifNothing userNotFound

-- if the user is expired, an internal event is handled asynchronously:
--   UserDeleted event to contacts
--   via galley: MemberLeave EdMembersLeave event to members for all conversations the user was in
getUserH :: N -> E -> JSON ::: UserId ::: OpaqueUserId -> Handler Response
getUserH N E (_ ::: self ::: uid) = do
  fmap json . ifNothing userNotFound =<< getUser N E self uid

-- if the user is expired, an internal event is handled asynchronously:
--   UserDeleted event to contacts
--   via galley: MemberLeave EdMembersLeave event to members for all conversations the user was in
getUser :: N -> E -> UserId -> OpaqueUserId -> Handler (Maybe UserProfile)
getUser N E self opaqueUserId = do
  resolvedUserId <- resolveOpaqueUserId opaqueUserId
  -- if the user is expired, an internal event is handled asynchronously:
  --   UserDeleted event to contacts
  --   via galley: MemberLeave EdMembersLeave event to members for all conversations the user was in
  lift $ API.lookupProfile N E self resolvedUserId

getUserNameH :: JSON ::: UserId -> Handler Response
getUserNameH (_ ::: self) = do
  name :: Maybe Name <- lift $ API.lookupName self
  return $ case name of
    Just n -> json $ object ["name" .= n]
    Nothing -> setStatus status404 empty

-- if the user is expired, an internal event is handled asynchronously:
--   UserDeleted event to contacts
--   via galley: MemberLeave EdMembersLeave event to members for all conversations the user was in
listUsersH :: N -> E -> JSON ::: UserId ::: Either (List OpaqueUserId) (List (OptionallyQualified Handle)) -> Handler Response
listUsersH N E (_ ::: self ::: qry) =
  toResponse <$> listUsers N E self qry
  where
    toResponse = \case
      [] -> setStatus status404 empty
      ps -> json ps

-- if the user is expired, an internal event is handled asynchronously:
--   UserDeleted event to contacts
--   via galley: MemberLeave EdMembersLeave event to members for all conversations the user was in
listUsers :: N -> E -> UserId -> Either (List OpaqueUserId) (List (OptionallyQualified Handle)) -> Handler [UserProfile]
listUsers N E self = \case
  Left us -> do
    resolvedUserIds <- traverse resolveOpaqueUserId (fromList us)
    -- UserDeleted event to contacts of expired users, after internal event is handled asynchronously
    byIds N E resolvedUserIds
  Right hs -> do
    us <- getIds (fromList hs)
    sameTeamSearchOnly <- fromMaybe False <$> view (settings . searchSameTeamOnly)
    -- UserDeleted event to contacts of expired users, after internal event is handled asynchronously
    if sameTeamSearchOnly
      then filterSameTeamOnly =<< byIds N E us
      else byIds N E us
  where
    getIds :: [OptionallyQualified Handle] -> Handler [MappedOrLocalId Id.U]
    getIds hs = do
      -- we might be able to do something smarter if the domain is our own
      let (localHandles, _remoteHandles) = partitionEithers (map eitherQualifiedOrNot hs)
      localUsers <- catMaybes <$> traverse (lift . API.lookupHandle) localHandles
      -- FUTUREWORK(federation): resolve remote handles, too
      pure (Local <$> localUsers)
    filterSameTeamOnly :: [UserProfile] -> Handler [UserProfile]
    filterSameTeamOnly us = do
      selfTeam <- lift $ Data.lookupUserTeam self
      return $ case selfTeam of
        Just team -> filter (\x -> profileTeam x == Just team) us
        Nothing -> us
    -- if the user is expired, an internal event is handled asynchronously:
    --   UserDeleted event to contacts
    --   via galley: MemberLeave EdMembersLeave event to members for all conversations the user was in
    byIds :: N -> E -> [MappedOrLocalId Id.U] -> Handler [UserProfile]
    byIds N E uids = lift $ API.lookupProfiles N E self uids

listActivatedAccountsH :: JSON ::: Either (List UserId) (List Handle) -> Handler Response
listActivatedAccountsH (_ ::: qry) = do
  json <$> lift (listActivatedAccounts qry)

listActivatedAccounts :: Either (List UserId) (List Handle) -> AppIO [UserAccount]
listActivatedAccounts = \case
  Left us -> byIds (fromList us)
  Right hs -> do
    us <- mapM (API.lookupHandle) (fromList hs)
    byIds (catMaybes us)
  where
    byIds uids =
      filter (isJust . userIdentity . accountUser)
        <$> API.lookupAccounts uids

listAccountsByIdentityH :: JSON ::: Either Email Phone -> Handler Response
listAccountsByIdentityH (_ ::: emailOrPhone) =
  lift $
    json
      <$> API.lookupAccountsByIdentity emailOrPhone

getActivationCodeH :: JSON ::: Either Email Phone -> Handler Response
getActivationCodeH (_ ::: emailOrPhone) = do
  json <$> getActivationCode emailOrPhone

getActivationCode :: Either Email Phone -> Handler GetActivationCodeResp
getActivationCode emailOrPhone = do
  apair <- lift $ API.lookupActivationCode emailOrPhone
  maybe (throwStd activationKeyNotFound) (return . GetActivationCodeResp) apair

data GetActivationCodeResp = GetActivationCodeResp (ActivationKey, ActivationCode)

instance ToJSON GetActivationCodeResp where
  toJSON (GetActivationCodeResp (k, c)) = object ["key" .= k, "code" .= c]

getPasswordResetCodeH :: JSON ::: Either Email Phone -> Handler Response
getPasswordResetCodeH (_ ::: emailOrPhone) = do
  maybe (throwStd invalidPwResetKey) (pure . json) =<< lift (getPasswordResetCode emailOrPhone)

getPasswordResetCode :: Either Email Phone -> AppIO (Maybe GetPasswordResetCodeResp)
getPasswordResetCode emailOrPhone = do
  GetPasswordResetCodeResp <$$> API.lookupPasswordResetCode emailOrPhone

data GetPasswordResetCodeResp = GetPasswordResetCodeResp (PasswordResetKey, PasswordResetCode)

instance ToJSON GetPasswordResetCodeResp where
  toJSON (GetPasswordResetCodeResp (k, c)) = object ["key" .= k, "code" .= c]

-- UserUpdated event to contacts
updateUserH :: E -> UserId ::: ConnId ::: JsonRequest UserUpdate -> Handler Response
updateUserH E (uid ::: conn ::: req) = do
  uu <- parseJsonBody req
  -- UserUpdated event to contacts
  lift $ API.updateUser E uid conn uu
  return empty

changeAccountStatusH :: UserId ::: JsonRequest AccountStatusUpdate -> Handler Response
changeAccountStatusH (usr ::: req) = do
  status <- suStatus <$> parseJsonBody req
  API.changeAccountStatus (List1.singleton usr) status !>> accountStatusError
  return empty

getAccountStatusH :: JSON ::: UserId -> Handler Response
getAccountStatusH (_ ::: usr) = do
  status <- lift $ API.lookupStatus usr
  return $ case status of
    Just s -> json $ AccountStatusResp s
    Nothing -> setStatus status404 empty

data AccountStatusResp = AccountStatusResp AccountStatus

instance ToJSON AccountStatusResp where
  toJSON (AccountStatusResp s) = object ["status" .= s]

changePhoneH :: UserId ::: ConnId ::: JsonRequest PhoneUpdate -> Handler Response
changePhoneH (u ::: c ::: req) = do
  setStatus status202 empty <$ (changePhone u c =<< parseJsonBody req)

changePhone :: UserId -> ConnId -> PhoneUpdate -> Handler ()
changePhone u _ (puPhone -> phone) = do
  (adata, pn) <- API.changePhone u phone !>> changePhoneError
  loc <- lift $ API.lookupLocale u
  let apair = (activationKey adata, activationCode adata)
  lift $ sendActivationSms pn apair loc

-- UserIdentityRemoved event to self
removePhoneH :: E -> UserId ::: ConnId -> Handler Response
removePhoneH E (self ::: conn) = do
  -- UserIdentityRemoved event to self
  API.removePhone E self conn !>> idtError
  return empty

-- UserIdentityRemoved event to self
removeEmailH :: E -> UserId ::: ConnId -> Handler Response
removeEmailH E (self ::: conn) = do
  -- UserIdentityRemoved event to self
  API.removeEmail E self conn !>> idtError
  return empty

checkPasswordExistsH :: UserId -> Handler Response
checkPasswordExistsH self = do
  exists <- lift $ isJust <$> API.lookupPassword self
  return $ if exists then empty else setStatus status404 empty

changePasswordH :: UserId ::: JsonRequest PasswordChange -> Handler Response
changePasswordH (u ::: req) = do
  cp <- parseJsonBody req
  API.changePassword u cp !>> changePwError
  return empty

changeLocaleH :: UserId ::: ConnId ::: JsonRequest LocaleUpdate -> Handler Response
changeLocaleH (u ::: conn ::: req) = do
  l <- parseJsonBody req
  lift $ API.changeLocale u conn l
  return empty

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
  if  | isJust owner ->
        -- Handle is taken (=> getHandleInfo will return 200)
        return CheckHandleFound
      | API.isBlacklistedHandle handle ->
        -- Handle is free but cannot be taken
        return CheckHandleInvalid
      | otherwise ->
        -- Handle is free and can be taken
        return CheckHandleNotFound

data CheckHandleResp = CheckHandleInvalid | CheckHandleFound | CheckHandleNotFound
  deriving (Eq, Show, Bounded, Enum, Generic)

checkHandlesH :: JSON ::: UserId ::: JsonRequest CheckHandles -> Handler Response
checkHandlesH (_ ::: _ ::: req) = do
  CheckHandles hs num <- parseJsonBody req
  let handles = mapMaybe parseHandle (fromRange hs)
  free <- lift $ API.checkHandles handles (fromRange num)
  return $ json free

getHandleInfoH :: JSON ::: UserId ::: Handle -> Handler Response
getHandleInfoH (_ ::: _ ::: h) = do
  owner <- lift $ API.lookupHandle h
  return $ case owner of
    Just u -> json (UserHandleInfo u)
    Nothing -> setStatus status404 empty

-- UserUpdated event to contacts
changeHandleH :: E -> UserId ::: ConnId ::: JsonRequest HandleUpdate -> Handler Response
changeHandleH E (u ::: conn ::: req) = do
  empty <$ (changeHandle E u conn =<< parseJsonBody req)

-- UserUpdated event to contacts
changeHandle :: E -> UserId -> ConnId -> HandleUpdate -> Handler ()
changeHandle E u conn (HandleUpdate h) = do
  handle <- validateHandle h
  -- UserUpdated event to contacts
  API.changeHandle E u conn handle !>> changeHandleError

beginPasswordResetH :: JSON ::: JsonRequest NewPasswordReset -> Handler Response
beginPasswordResetH (_ ::: req) = do
  setStatus status201 empty <$ (beginPasswordReset =<< parseJsonBody req)

beginPasswordReset :: NewPasswordReset -> Handler ()
beginPasswordReset (NewPasswordReset target) = do
  checkWhitelist target
  (u, pair) <- API.beginPasswordReset target !>> pwResetError
  loc <- lift $ API.lookupLocale u
  lift $ case target of
    Left email -> sendPasswordResetMail email pair loc
    Right phone -> sendPasswordResetSms phone pair loc

completePasswordResetH :: JSON ::: JsonRequest CompletePasswordReset -> Handler Response
completePasswordResetH (_ ::: req) = do
  CompletePasswordReset {..} <- parseJsonBody req
  API.completePasswordReset cpwrIdent cpwrCode cpwrPassword !>> pwResetError
  return empty

sendActivationCodeH :: JsonRequest SendActivationCode -> Handler Response
sendActivationCodeH req = do
  empty <$ (sendActivationCode =<< parseJsonBody req)

-- docs/reference/user/activation.md {#RefActivationRequest}
sendActivationCode :: SendActivationCode -> Handler ()
sendActivationCode SendActivationCode {..} = do
  checkWhitelist saUserKey
  API.sendActivationCode saUserKey saLocale saCall !>> sendActCodeError

changeSelfEmailH :: UserId ::: ConnId ::: JsonRequest EmailUpdate -> Handler Response
changeSelfEmailH (u ::: _ ::: req) = changeEmail u req True

getConnectionsStatusH ::
  JSON ::: JsonRequest ConnectionsStatusRequest ::: Maybe Relation ->
  Handler Response
getConnectionsStatusH (_ ::: req ::: flt) = do
  body <- parseJsonBody req
  json <$> lift (getConnectionsStatus body flt)

getConnectionsStatus :: ConnectionsStatusRequest -> Maybe Relation -> AppIO [ConnectionStatus]
getConnectionsStatus ConnectionsStatusRequest {csrFrom, csrTo} flt = do
  r <- API.lookupConnectionStatus csrFrom csrTo
  return $ maybe r (filterByRelation r) flt
  where
    filterByRelation l rel = filter ((== rel) . csStatus) l

-- ConnectionUpdated event to self and other, unless both states already exist and (1) any state is blocked or (2) both sides already accepted
--
-- potentially can cause events to be sent from Galley:
-- if only the other already was member of connect conversation and has connection status Sent or Accepted:
--   via galley: MemberJoin EdMembersJoin event to you and other
--
--  Also possible (for details check 'Galley.API.Create.createConnectConversation'):
--   via galley: ConvCreate EdConversation event to self
--   via galley: ConvConnect EdConnect event to self
createConnectionH :: N -> E -> JSON ::: UserId ::: ConnId ::: JsonRequest ConnectionRequest -> Handler Response
createConnectionH N E (_ ::: self ::: conn ::: req) = do
  cr <- parseJsonBody req
  -- ConnectionUpdated event to self, unless both states already exist and any state is blocked or both already accepted
  -- ConnectionUpdated event to other, unless both states already exist and any state is blocked or both already accepted
  --
  -- potentially can cause events to be sent from Galley:
  -- if only the other already was member of connect conversation and has connection status Sent or Accepted:
  --   via galley: MemberJoin EdMembersJoin event to you and other
  --
  --  Also possible (for details check 'Galley.API.Create.createConnectConversation'):
  --   via galley: ConvCreate EdConversation event to self
  --   via galley: ConvConnect EdConnect event to self
  rs <- API.createConnection N E self cr conn !>> connError
  return $ case rs of
    ConnectionCreated c -> setStatus status201 $ json c
    ConnectionExists c -> json c

-- ConnectionUpdated event to self, if our state changes
-- ConnectionUpdated event to other, if their state changes as well
--
-- when moving to Sent or Accepted, this potentially can cause events to be sent from Galley when joining the connect conversation:
--   via galley: MemberJoin EdMembersJoin event to you
--   via galley: MemberJoin EdMembersJoin event to other
updateConnectionH :: N -> E -> JSON ::: UserId ::: ConnId ::: UserId ::: JsonRequest ConnectionUpdate -> Handler Response
updateConnectionH N E (_ ::: self ::: conn ::: other ::: req) = do
  newStatus <- cuStatus <$> parseJsonBody req
  -- ConnectionUpdated event to self, if our state changes
  -- ConnectionUpdated event to other, if their state changes as well
  mc <- API.updateConnection N E self other newStatus (Just conn) !>> connError
  return $ case mc of
    Just c -> json c
    Nothing -> setStatus status204 empty

listConnectionsH :: JSON ::: UserId ::: Maybe UserId ::: Range 1 500 Int32 -> Handler Response
listConnectionsH (_ ::: uid ::: start ::: size) =
  json
    <$> lift (API.lookupConnections uid start size)

getConnectionH :: JSON ::: UserId ::: UserId -> Handler Response
getConnectionH (_ ::: uid ::: uid') = lift $ do
  conn <- API.lookupConnection uid uid'
  return $ case conn of
    Just c -> json c
    Nothing -> setStatus status404 empty

-- UserIdentityRemoved event to self
revokeIdentityH :: E -> Either Email Phone -> Handler Response
revokeIdentityH E emailOrPhone = do
  -- UserIdentityRemoved event to self
  lift $ API.revokeIdentity E emailOrPhone
  return $ setStatus status200 empty

checkBlacklistH :: Either Email Phone -> Handler Response
checkBlacklistH emailOrPhone = do
  bl <- lift $ API.isBlacklisted emailOrPhone
  return $ setStatus (bool status404 status200 bl) empty

deleteFromBlacklistH :: Either Email Phone -> Handler Response
deleteFromBlacklistH emailOrPhone = do
  void . lift $ API.blacklistDelete emailOrPhone
  return empty

addBlacklistH :: Either Email Phone -> Handler Response
addBlacklistH emailOrPhone = do
  void . lift $ API.blacklistInsert emailOrPhone
  return empty

-- | Get any matching prefixes. Also try for shorter prefix matches,
-- i.e. checking for +123456 also checks for +12345, +1234, ...
getPhonePrefixesH :: PhonePrefix -> Handler Response
getPhonePrefixesH prefix = do
  results <- lift $ API.phonePrefixGet prefix
  return $ case results of
    [] -> setStatus status404 empty
    _ -> json results

-- | Delete a phone prefix entry (must be an exact match)
deleteFromPhonePrefixH :: PhonePrefix -> Handler Response
deleteFromPhonePrefixH prefix = do
  void . lift $ API.phonePrefixDelete prefix
  return empty

addPhonePrefixH :: JSON ::: JsonRequest ExcludedPrefix -> Handler Response
addPhonePrefixH (_ ::: req) = do
  prefix :: ExcludedPrefix <- parseJsonBody req
  void . lift $ API.phonePrefixInsert prefix
  return empty

canBeDeletedH :: UserId ::: TeamId -> Handler Response
canBeDeletedH (uid ::: tid) = do
  onlyOwner <- lift (Team.teamOwnershipStatus uid tid)
  if canBeDeleted onlyOwner
    then pure empty
    else throwStd noOtherOwner

canBeDeleted :: Team.TeamOwnershipStatus -> Bool
canBeDeleted = \case
  Team.IsOnlyTeamOwnerWithEmail -> False
  Team.IsOneOfManyTeamOwnersWithEmail -> True
  Team.IsTeamOwnerWithoutEmail -> True
  Team.IsNotTeamOwner -> True

isTeamOwnerH :: UserId ::: TeamId -> Handler Response
isTeamOwnerH (uid ::: tid) = do
  onlyOwner <- lift (Team.teamOwnershipStatus uid tid)
  if isTeamOwner onlyOwner
    then pure empty
    else throwStd insufficientTeamPermissions

isTeamOwner :: Team.TeamOwnershipStatus -> Bool
isTeamOwner = \case
  Team.IsOnlyTeamOwnerWithEmail -> True
  Team.IsOneOfManyTeamOwnersWithEmail -> True
  Team.IsTeamOwnerWithoutEmail -> True
  Team.IsNotTeamOwner -> False

updateSSOIdH :: UserId ::: JSON ::: JsonRequest UserSSOId -> Handler Response
updateSSOIdH (uid ::: _ ::: req) = do
  ssoid :: UserSSOId <- parseJsonBody req
  success <- lift $ Data.updateSSOId uid ssoid
  if success
    then return empty
    else return . setStatus status404 $ plain "User does not exist or has no team."

updateManagedByH :: UserId ::: JSON ::: JsonRequest ManagedByUpdate -> Handler Response
updateManagedByH (uid ::: _ ::: req) = do
  ManagedByUpdate managedBy <- parseJsonBody req
  lift $ Data.updateManagedBy uid managedBy
  return empty

updateRichInfoH :: UserId ::: JSON ::: JsonRequest RichInfoUpdate -> Handler Response
updateRichInfoH (uid ::: _ ::: req) = do
  empty <$ (updateRichInfo uid =<< parseJsonBody req)

updateRichInfo :: UserId -> RichInfoUpdate -> Handler ()
updateRichInfo uid rup = do
  let RichInfoAssocList richInfo = normalizeRichInfoAssocList . riuRichInfo $ rup
  maxSize <- setRichInfoLimit <$> view settings
  when (richInfoAssocListSize richInfo > maxSize) $ throwStd tooLargeRichInfo
  lift $ Data.updateRichInfo uid (RichInfoAssocList richInfo)

-- FUTUREWORK: send an event
-- Intra.onUserEvent uid (Just conn) (richInfoUpdate uid ri)

-- UserDeleted event to contacts
-- via galley: MemberLeave EdMembersLeave event to members for all conversations the user was in
deleteUserH :: N -> E -> UserId ::: JsonRequest DeleteUser ::: JSON -> Handler Response
deleteUserH N E (u ::: r ::: _) = do
  body <- parseJsonBody r
  -- UserDeleted event to contacts
  -- via galley: MemberLeave EdMembersLeave event to members for all conversations the user was in
  res <- API.deleteUser N E u (deleteUserPassword body) !>> deleteUserError
  return $ case res of
    Nothing -> setStatus status200 empty
    Just ttl -> setStatus status202 (json (DeletionCodeTimeout ttl))

-- UserDeleted event to contacts
-- via galley: MemberLeave EdMembersLeave event to members for all conversations the user was in
verifyDeleteUserH :: N -> E -> JsonRequest VerifyDeleteUser ::: JSON -> Handler Response
verifyDeleteUserH N E (r ::: _) = do
  body <- parseJsonBody r
  -- UserDeleted event to contacts
  -- via galley: MemberLeave EdMembersLeave event to members for all conversations the user was in
  API.verifyDeleteUser N E body !>> deleteUserError
  return (setStatus status200 empty)

getContactListH :: JSON ::: UserId -> Handler Response
getContactListH (_ ::: uid) = do
  contacts <- lift $ API.lookupContactList uid
  return $ json $ (UserIds contacts)

-- activation

-- docs/reference/user/activation.md {#RefActivationSubmit}
--
-- UserActivated event to self, if account gets activated
-- UserIdentityUpdated event to self, if email or phone get activated
activateKeyH :: E -> JSON ::: JsonRequest Activate -> Handler Response
activateKeyH E (_ ::: req) = respFromActivationRespWithStatus <$> (activate E =<< parseJsonBody req)

-- UserActivated event to self, if account gets activated
-- UserIdentityUpdated event to self, if email or phone get activated
activateH :: E -> ActivationKey ::: ActivationCode -> Handler Response
activateH E (k ::: c) = respFromActivationRespWithStatus <$> (activate E (Activate (ActivateKey k) c False))

-- UserActivated event to self, if account gets activated
-- UserIdentityUpdated event to self, if email or phone get activated
activate :: E -> Activate -> Handler ActivationRespWithStatus
activate E (Activate tgt code dryrun)
  | dryrun = do
    API.preverify tgt code !>> actError
    return $ ActivationRespDryRun
  | otherwise = do
    -- UserActivated event to self, if account gets activated
    -- UserIdentityUpdated event to self, if email or phone get activated
    result <- API.activate E tgt code Nothing !>> actError
    return $ case result of
      ActivationSuccess ident first -> respond ident first
      ActivationPass -> ActivationRespPass
  where
    respond (Just ident) first = ActivationResp $ ActivationResponse ident first
    respond Nothing _ = ActivationRespSuccessNoIdent

data ActivationRespWithStatus
  = ActivationResp ActivationResponse
  | ActivationRespDryRun
  | ActivationRespPass
  | ActivationRespSuccessNoIdent

respFromActivationRespWithStatus :: ActivationRespWithStatus -> Response
respFromActivationRespWithStatus = \case
  ActivationResp aresp -> json aresp
  ActivationRespDryRun -> empty
  ActivationRespPass -> setStatus status204 empty
  ActivationRespSuccessNoIdent -> empty

-- Deprecated

-- Deprecated and to be removed after new versions of brig and galley are
-- deployed. Reason for deprecation: it returns N^2 things (which is not
-- needed), it doesn't scale, and it accepts everything in URL parameters,
-- which doesn't work when the list of users is long.
deprecatedGetConnectionsStatusH :: List UserId ::: Maybe Relation -> Handler Response
deprecatedGetConnectionsStatusH (users ::: flt) = do
  r <- lift $ API.lookupConnectionStatus (fromList users) (fromList users)
  return . json $ maybe r (filterByRelation r) flt
  where
    filterByRelation l rel = filter ((== rel) . csStatus) l

deprecatedOnboardingH :: JSON ::: UserId ::: JsonRequest Value -> Handler Response
deprecatedOnboardingH (_ ::: _ ::: _) = pure $ json DeprecatedMatchingResult

data DeprecatedMatchingResult = DeprecatedMatchingResult

instance ToJSON DeprecatedMatchingResult where
  toJSON DeprecatedMatchingResult =
    object
      [ "results" .= ([] :: [()]),
        "auto-connects" .= ([] :: [()])
      ]

deprecatedCompletePasswordResetH :: JSON ::: PasswordResetKey ::: JsonRequest PasswordReset -> Handler Response
deprecatedCompletePasswordResetH (_ ::: k ::: req) = do
  pwr <- parseJsonBody req
  API.completePasswordReset (PasswordResetIdentityKey k) (pwrCode pwr) (pwrPassword pwr) !>> pwResetError
  return empty

-- Utilities

changeEmail :: UserId -> JsonRequest EmailUpdate -> Bool -> Handler Response
changeEmail u req sendOutEmail = do
  email <- euEmail <$> parseJsonBody req
  API.changeEmail u email !>> changeEmailError >>= \case
    ChangeEmailIdempotent -> respond status204
    ChangeEmailNeedsActivation (usr, adata, en) -> handleActivation usr adata en
  where
    respond = return . flip setStatus empty
    handleActivation usr adata en = do
      when sendOutEmail $ do
        let apair = (activationKey adata, activationCode adata)
        let name = userName usr
        let ident = userIdentity usr
        let lang = userLocale usr
        lift $ sendActivationMail en name apair (Just lang) ident
      respond status202

validateHandle :: Text -> Handler Handle
validateHandle = maybe (throwE (StdError invalidHandle)) return . parseHandle

ifNothing :: Utilities.Error -> Maybe a -> Handler a
ifNothing e = maybe (throwStd e) return

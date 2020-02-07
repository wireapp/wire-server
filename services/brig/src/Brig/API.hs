{-# LANGUAGE RecordWildCards   #-}

module Brig.API (sitemap) where

import Imports hiding (head)
import Brig.App
import Brig.API.Error
import Brig.API.Handler
import Brig.API.Types
import Brig.Options hiding (sesQueue, internalEvents)
import Brig.Types
import Brig.Types.Intra
import Brig.Types.User (NewUserPublic(NewUserPublic))
import Brig.Types.User.Auth
import Brig.Types.Team.LegalHold (LegalHoldClientRequest(..))
import Brig.User.Email
import Brig.User.Phone
import Control.Error hiding (bool)
import Control.Lens (view, (^.))
import Data.Aeson hiding (json)
import Data.ByteString.Conversion
import Data.Id
import Data.Misc (IpAddr (..), (<$$>))
import Data.Range
import Data.Text.Encoding (decodeLatin1)
import Data.Text.Lazy (pack)
import Galley.Types (UserClients (..), UserClientMap(..))
import Network.HTTP.Types.Status
import Network.Wai (Response, lazyRequestBody)
import Network.Wai.Predicate hiding (setStatus, result)
import Network.Wai.Routing
import Network.Wai.Utilities
import Network.Wai.Utilities.Response (json)
import Network.Wai.Utilities.Swagger (document, mkSwaggerApi)

import qualified Data.Text.Ascii               as Ascii
import qualified Data.List1                    as List1
import qualified Brig.API.Client               as API
import qualified Brig.API.Connection           as API
import qualified Brig.API.Properties           as API
import qualified Brig.API.User                 as API
import qualified Brig.Data.User                as Data
import qualified Brig.Team.Util                as Team
import qualified Brig.User.API.Auth            as Auth
import qualified Brig.User.API.Search          as Search
import qualified Brig.User.Auth.Cookie         as Auth
import qualified Brig.Types.Swagger            as Doc
import qualified Network.Wai.Utilities.Swagger as Doc
import qualified Data.Swagger.Build.Api        as Doc
import qualified Galley.Types.Swagger          as Doc
import qualified Galley.Types.Teams            as Team
import qualified Network.Wai.Utilities         as Utilities
import qualified Data.ByteString.Lazy          as Lazy
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import qualified Data.Text                     as Text
import qualified Brig.Provider.API             as Provider
import qualified Brig.Team.API                 as Team
import qualified Brig.Team.Email               as Team
import qualified Brig.TURN.API                 as TURN
import qualified Data.ZAuth.Token              as ZAuth

---------------------------------------------------------------------------
-- Sitemap

sitemap :: Opts -> Routes Doc.ApiBuilder Handler ()
sitemap o = do

    -- Internal ---------------------------------------------------------------

    get  "/i/status" (continue $ const $ return empty) true
    head "/i/status" (continue $ const $ return empty) true

    post "/i/users/:uid/auto-connect" (continue autoConnectH) $
        accept "application" "json"
        .&. capture "uid"
        .&. opt (header "Z-Connection")
        .&. jsonRequest @UserSet

    post "/i/users" (continue createUserNoVerifyH) $
        accept "application" "json"
        .&. jsonRequest @NewUser

    put "/i/self/email" (continue changeSelfEmailNoSendH) $
        header "Z-User"
        .&. jsonRequest @EmailUpdate

    delete "/i/users/:uid" (continue deleteUserNoVerifyH) $
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

    post "/i/users/revoke-identity" (continue revokeIdentityH) $
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

    post "/i/clients/:uid" (continue addClientInternalH) $
      capture "uid"
      .&. jsonRequest @NewClient
      .&. opt (header "Z-Connection")
      .&. accept "application" "json"

    post "/i/clients/legalhold/:uid/request" (continue legalHoldClientRequestedH) $
      capture "uid"
      .&. jsonRequest @LegalHoldClientRequest
      .&. accept "application" "json"

    delete "/i/clients/legalhold/:uid" (continue removeLegalHoldClientH) $
      capture "uid"
      .&. accept "application" "json"

    -- /users -----------------------------------------------------------------

    get "/users/api-docs"
        (\(_ ::: url) k ->
            let doc = mkSwaggerApi (decodeLatin1 url) Doc.brigModels (sitemap o)
            in k $ json doc) $
        accept "application" "json"
        .&. query "base_url"

    ---

    head "/users/:uid" (continue checkUserExistsH) $
        header "Z-User"
        .&. capture "uid"

    document "HEAD" "userExists" $ do
        Doc.summary "Check if a user ID exists"
        Doc.parameter Doc.Path "uid" Doc.bytes' $
            Doc.description "User ID"
        Doc.response 200 "User exists" Doc.end
        Doc.errorResponse userNotFound

    ---

    get "/users/:uid" (continue getUserH) $
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

    get "/users" (continue listUsersH) $
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
        Doc.summary "Given a map of user IDs to client IDs return a \
        \prekey for each one. You can't request information for more users than \
        \maximum conversation size."
        Doc.notes "Prekeys of all clients of a multiple users. \
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

    put "/self" (continue updateUserH) $
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

    put "/self/handle" (continue changeHandleH) $
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

    delete "/self/phone" (continue removePhoneH) $
        header "Z-User"
        .&. header "Z-Connection"

    document "DELETE" "removePhone" $ do
        Doc.summary "Remove your phone number."
        Doc.notes "Your phone number can only be removed if you also have an \
                  \email address and a password."
        Doc.response 200 "Phone number removed." Doc.end
        Doc.errorResponse lastIdentity
        Doc.errorResponse noPassword

    ---

    delete "/self/email" (continue removeEmailH) $
        header "Z-User"
        .&. header "Z-Connection"

    document "DELETE" "removeEmail" $ do
        Doc.summary "Remove your email address."
        Doc.notes "Your email address can only be removed if you also have a \
                  \phone number."
        Doc.response 200 "Email address removed." Doc.end
        Doc.errorResponse lastIdentity


    ---
    ---

    delete "/self" (continue deleteUserH) $
        header "Z-User"
        .&. jsonRequest @DeleteUser
        .&. accept "application" "json"

    document "DELETE" "deleteUser" $ do
        Doc.summary "Initiate account deletion."
        Doc.notes "If the account has a verified identity, a verification \
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

    post "/delete" (continue verifyDeleteUserH) $
        jsonRequest @VerifyDeleteUser
        .&. accept "application" "json"

    document "POST" "verifyDeleteUser" $ do
        Doc.summary "Verify account deletion with a code."
        Doc.body (Doc.ref Doc.verifyDelete) $
            Doc.description "JSON body"
        Doc.response 200 "Deletion is initiated." Doc.end
        Doc.errorResponse invalidCode

    ---

    post "/connections" (continue createConnectionH) $
        accept "application" "json"
        .&. header "Z-User"
        .&. header "Z-Connection"
        .&. jsonRequest @ConnectionRequest

    document "POST" "createConnection" $ do
        Doc.summary "Create a connection to another user."
        Doc.notes $ "You can have no more than "
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

    put "/connections/:id" (continue updateConnectionH) $
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

    post "/clients" (continue addClientH) $
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

    delete "/clients/:client" (continue rmClientH) $
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

    put "/properties/:key" (continue setPropertyH) $
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

    delete "/properties/:key" (continue deletePropertyH) $
        header "Z-User"
        .&. header "Z-Connection"
        .&. capture "key"

    document "DELETE" "deleteProperty" $ do
        Doc.summary "Delete a property."
        Doc.parameter Doc.Path "key" Doc.string' $
            Doc.description "Property key"
        Doc.response 200 "Property deleted." Doc.end

    ---

    delete "/properties" (continue clearPropertiesH) $
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
    post "/register" (continue createUserH) $
        accept "application" "json"
        .&. jsonRequest @NewUserPublic

    document "POST" "register" $ do
        Doc.summary "Register a new user."
        Doc.notes   "If the environment where the registration takes \
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
        Doc.returns (Doc.ref Doc.activationResponse)
        Doc.response 200 "Activation successful." Doc.end
        Doc.response 204 "A recent activation was already successful." Doc.end
        Doc.errorResponse activationCodeNotFound

    ---

    -- docs/reference/user/activation.md {#RefActivationSubmit}
    post "/activate" (continue activateKeyH) $
        accept "application" "json"
        .&. jsonRequest @Activate

    document "POST" "activate" $ do
        Doc.summary "Activate (i.e. confirm) an email address or phone number."
        Doc.notes "Activation only succeeds once and the number of \
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

    post "/onboarding/v3" (continue onboardingH) $
        accept "application" "json"
        .&. header "Z-User"
        .&. jsonRequest @AddressBook

    document "POST" "onboardingV3" $ do
        Doc.summary "Upload contacts and invoke matching. Returns the list of Matches"
        Doc.body (Doc.ref Doc.addressBook) $ Doc.description "Address book"
        Doc.returns (Doc.ref Doc.onboardingMatches)
        Doc.response 200 "Matches" Doc.end

    -----

    Provider.routes
    Auth.routes
    Search.routes
    Team.routes
    TURN.routes

---------------------------------------------------------------------------
-- Handlers

setPropertyH :: UserId ::: ConnId ::: PropertyKey ::: JsonRequest PropertyValue -> Handler Response
setPropertyH (u ::: c ::: k ::: req) = do
    empty <$ (setProperty u c k =<< lazyParsePropertyValue (lazyRequestBody (fromJsonRequest req)))

setProperty :: UserId -> ConnId -> PropertyKey -> PropertyValue -> Handler ()
setProperty u c k propval = do
    maxKeyLen <- fromMaybe defMaxKeyLen <$> view (settings . propertyMaxKeyLen)
    unless (Text.compareLength (Ascii.toText (propertyKeyName k)) (fromIntegral maxKeyLen) <= EQ) $
        throwStd propertyKeyTooLarge
    API.setProperty u c k propval !>> propDataError

-- | Parse a 'PropertyValue' from a bytestring.  This is different from 'FromJSON' in that
-- checks the byte size of the input, and fails *without consuming all of it* if that size
-- exceeds the settings.
lazyParsePropertyValue :: IO Lazy.ByteString -> Handler PropertyValue
lazyParsePropertyValue lreqbody = do
    maxValueLen <- fromMaybe defMaxValueLen <$> view (settings . propertyMaxValueLen)
    lbs <- Lazy.take (maxValueLen + 1) <$> liftIO lreqbody
    unless (Lazy.length lbs <= maxValueLen) $
        throwStd propertyValueTooLarge
    hoistEither $ fmapL (StdError . badRequest . pack) (eitherDecode lbs)

deletePropertyH :: UserId ::: ConnId ::: PropertyKey -> Handler Response
deletePropertyH (u ::: c ::: k) = lift (API.deleteProperty u c k) >> return empty

clearPropertiesH :: UserId ::: ConnId -> Handler Response
clearPropertiesH (u ::: c) = lift (API.clearProperties u c) >> return empty

getPropertyH :: UserId ::: PropertyKey ::: JSON -> Handler Response
getPropertyH (u ::: k ::: _) = do
    val <- lift $ API.lookupProperty u k
    return $ case val of
        Nothing -> setStatus status404 empty
        Just  v -> json v

listPropertyKeysH :: UserId ::: JSON -> Handler Response
listPropertyKeysH (u ::: _) = json <$> lift (API.lookupPropertyKeys u)

listPropertyKeysAndValuesH :: UserId ::: JSON -> Handler Response
listPropertyKeysAndValuesH (u ::: _) = json <$> lift (API.lookupPropertyKeysAndValues u)

getPrekeyH :: UserId ::: ClientId ::: JSON -> Handler Response
getPrekeyH (u ::: c ::: _) = do
    prekey <- lift $ API.claimPrekey u c
    return $ case prekey of
        Just pk -> json pk
        Nothing -> setStatus status404 empty

getPrekeyBundleH :: UserId ::: JSON -> Handler Response
getPrekeyBundleH (u ::: _) = json <$> lift (API.claimPrekeyBundle u)

getMultiPrekeyBundlesH :: JsonRequest UserClients ::: JSON -> Handler Response
getMultiPrekeyBundlesH (req ::: _) = do
    json <$> (getMultiPrekeyBundles =<< parseJsonBody req)

getMultiPrekeyBundles :: UserClients -> Handler (UserClientMap (Maybe Prekey))
getMultiPrekeyBundles body = do
    maxSize <- fromIntegral . setMaxConvSize <$> view settings
    when (Map.size (userClients body) > maxSize) $
        throwStd tooManyClients
    lift (API.claimMultiPrekeyBundles body)

addClientH :: JsonRequest NewClient ::: UserId ::: ConnId ::: Maybe IpAddr ::: JSON -> Handler Response
addClientH (req ::: usr ::: con ::: ip ::: _) = do
    new <- parseJsonBody req
    clt <- addClient new usr con ip
    let loc = toByteString' $ clientId clt
    pure . setStatus status201 . addHeader "Location" loc . json $ clt

addClient :: NewClient -> UserId -> ConnId -> Maybe IpAddr -> Handler Client
addClient new usr con ip = do
    -- Users can't add legal hold clients
    when (newClientType new == LegalHoldClientType)
        $ throwE (clientError ClientLegalHoldCannotBeAdded)
    API.addClient usr (Just con) (ipAddr <$> ip) new !>> clientError

-- | Add a client without authentication checks
addClientInternalH :: UserId ::: JsonRequest NewClient ::: Maybe ConnId ::: JSON -> Handler Response
addClientInternalH (usr ::: req ::: connId ::: _) = do
    new <- parseJsonBody req
    setStatus status201 . json <$> addClientInternal usr new connId

addClientInternal :: UserId -> NewClient -> Maybe ConnId -> Handler Client
addClientInternal usr new connId = do
    API.addClient usr connId Nothing new !>> clientError

rmClientH :: JsonRequest RmClient ::: UserId ::: ConnId ::: ClientId ::: JSON -> Handler Response
rmClientH (req ::: usr ::: con ::: clt ::: _) = do
    body <- parseJsonBody req
    empty <$ rmClient body usr con clt

rmClient :: RmClient -> UserId -> ConnId -> ClientId -> Handler ()
rmClient body usr con clt = do
    API.rmClient usr con clt (rmPassword body) !>> clientError

legalHoldClientRequestedH :: UserId ::: JsonRequest LegalHoldClientRequest ::: JSON -> Handler Response
legalHoldClientRequestedH (targetUser ::: req ::: _) = do
    clientRequest <- parseJsonBody req
    lift $ API.legalHoldClientRequested targetUser clientRequest
    return $ setStatus status200 empty

removeLegalHoldClientH :: UserId ::: JSON -> Handler Response
removeLegalHoldClientH (uid ::: _) = do
    lift $ API.removeLegalHoldClient uid
    return $ setStatus status200 empty

updateClientH :: JsonRequest UpdateClient ::: UserId ::: ClientId ::: JSON -> Handler Response
updateClientH (req ::: usr ::: clt ::: _) = do
    body <- parseJsonBody req
    empty <$ updateClient body usr clt

updateClient :: UpdateClient -> UserId -> ClientId -> Handler ()
updateClient body usr clt = do
    API.updateClient usr clt body !>> clientError

listClientsH :: UserId ::: JSON -> Handler Response
listClientsH (usr ::: _) = json <$> lift (API.lookupClients usr)

internalListClientsH :: JSON ::: JsonRequest UserSet -> Handler Response
internalListClientsH (_ ::: req) = do
    json <$> (lift . internalListClients =<< parseJsonBody req)

internalListClients :: UserSet -> AppIO UserClients
internalListClients (UserSet usrs) = do
    UserClients . Map.fromList <$> (API.lookupUsersClientIds $ Set.toList usrs)

getClientH :: UserId ::: ClientId ::: JSON -> Handler Response
getClientH (usr ::: clt ::: _) = lift $ do
    client <- API.lookupClient usr clt
    return $ case client of
        Just c  -> json c
        Nothing -> setStatus status404 empty

getUserClientsH :: UserId ::: JSON -> Handler Response
getUserClientsH (user ::: _) =
    json <$> lift (getUserClients user)

getUserClients :: UserId -> AppIO [PubClient]
getUserClients user =
    API.pubClient <$$> API.lookupClients user

getUserClientH :: UserId ::: ClientId ::: JSON -> Handler Response
getUserClientH (user ::: cid ::: _) = do
    maybe (setStatus status404 empty) json <$> lift (getUserClient user cid)

getUserClient :: UserId -> ClientId -> AppIO (Maybe PubClient)
getUserClient user cid = do
    API.pubClient <$$> API.lookupClient user cid

getRichInfoH :: UserId ::: UserId ::: JSON -> Handler Response
getRichInfoH (self ::: user ::: _) = do
    json <$> getRichInfo self user

getRichInfo :: UserId -> UserId -> Handler RichInfoAssocList
getRichInfo self user = do
    -- Check that both users exist and the requesting user is allowed to see rich info of the
    -- other user
    selfUser  <- ifNothing userNotFound =<< lift (Data.lookupUser self)
    otherUser <- ifNothing userNotFound =<< lift (Data.lookupUser user)
    case (userTeam selfUser, userTeam otherUser) of
        (Just t1, Just t2) | t1 == t2 -> pure ()
        _ -> throwStd insufficientTeamPermissions
    -- Query rich info
    fromMaybe emptyRichInfoAssocList <$> lift (API.lookupRichInfo user)

listPrekeyIdsH :: UserId ::: ClientId ::: JSON -> Handler Response
listPrekeyIdsH (usr ::: clt ::: _) = json <$> lift (API.lookupPrekeyIds usr clt)

autoConnectH :: JSON ::: UserId ::: Maybe ConnId ::: JsonRequest UserSet -> Handler Response
autoConnectH (_ ::: uid ::: conn ::: req) = do
    UserSet to <- parseJsonBody req
    let num = Set.size to
    when (num < 1) $
        throwStd $ badRequest "No users given for auto-connect."
    when (num > 25) $
        throwStd $ badRequest "Too many users given for auto-connect (> 25)."
    conns <- API.autoConnect uid to conn !>> connError
    return $ json conns

-- docs/reference/user/registration.md {#RefRegistration}
createUserH :: JSON ::: JsonRequest NewUserPublic -> Handler Response
createUserH (_ ::: req) = do
    NewUserPublic new <- parseJsonBody req
    for_ (newUserEmail new) $ checkWhitelist . Left
    for_ (newUserPhone new) $ checkWhitelist . Right
    result <- API.createUser new !>> newUserError
    let acc   = createdAccount result
    let usr   = accountUser acc
    let eac   = createdEmailActivation result
    let pac   = createdPhoneActivation result
    let epair = (,) <$> (activationKey <$> eac) <*> (activationCode <$> eac)
    let ppair = (,) <$> (activationKey <$> pac) <*> (activationCode <$> pac)
    let lang  = userLocale usr
    lift $ do
        for_ (liftM2 (,) (userEmail usr) epair) $ \(e, p) ->
            sendActivationEmail e (userName usr) p (Just lang) (newUserTeam new)
        for_ (liftM2 (,) (userPhone usr) ppair) $ \(p, c) ->
            sendActivationSms p c (Just lang)
        for_ (liftM3 (,,) (userEmail usr) (createdUserTeam result) (newUserTeam new)) $ \(e, ct, ut) ->
            sendWelcomeEmail e ct ut (Just lang)
    cok <- case acc of
        UserAccount _ Ephemeral -> lift $ Auth.newCookie @ZAuth.User (userId usr) SessionCookie (newUserLabel new)
        UserAccount _ _         -> lift $ Auth.newCookie @ZAuth.User (userId usr) PersistentCookie (newUserLabel new)
    lift $ Auth.setResponseCookie cok
        $ setStatus status201
        . addHeader "Location" (toByteString' (userId usr))
        $ json (SelfProfile usr)
  where
    sendActivationEmail e u p l (Just (NewTeamCreator (BindingNewTeamUser (Team.BindingNewTeam t) _))) =
        sendTeamActivationMail e u p l (fromRange $ t^.Team.newTeamName)
    sendActivationEmail e u p l _ =
        sendActivationMail e u p l Nothing

    sendWelcomeEmail :: Email -> CreateUserTeam -> NewTeamUser -> Maybe Locale -> AppIO ()
    -- NOTE: Welcome e-mails for the team creator are not dealt by brig anymore
    sendWelcomeEmail _ (CreateUserTeam _ _) (NewTeamCreator _) _ = return ()
    sendWelcomeEmail e (CreateUserTeam t n) (NewTeamMember  _) l = Team.sendMemberWelcomeMail e t n l
    sendWelcomeEmail e (CreateUserTeam t n) (NewTeamMemberSSO _) l = Team.sendMemberWelcomeMail e t n l

createUserNoVerifyH :: JSON ::: JsonRequest NewUser -> Handler Response
createUserNoVerifyH (_ ::: req) = do
    (uData :: NewUser)  <- parseJsonBody req
    result <- API.createUser uData !>> newUserError
    let acc = createdAccount result
    let usr = accountUser acc
    let uid = userId usr
    let eac = createdEmailActivation result
    let pac = createdPhoneActivation result
    for_ (catMaybes [eac, pac]) $ \adata ->
        let key  = ActivateKey $ activationKey adata
            code = activationCode adata
        in API.activate key code (Just uid) !>> actError
    return . setStatus status201
           . addHeader "Location" (toByteString' uid)
           $ json (SelfProfile usr)

deleteUserNoVerifyH :: UserId -> Handler Response
deleteUserNoVerifyH uid = do
    void $ lift (API.lookupAccount uid) >>= ifNothing userNotFound
    lift $ API.deleteUserNoVerify uid
    return $ setStatus status202 empty

changeSelfEmailNoSendH :: UserId ::: JsonRequest EmailUpdate -> Handler Response
changeSelfEmailNoSendH (u ::: req) = changeEmail u req False

checkUserExistsH :: UserId ::: UserId -> Handler Response
checkUserExistsH (self ::: uid) = do
    exists <- lift $ isJust <$> API.lookupProfile self uid
    if exists then return empty else throwStd userNotFound

getSelfH :: JSON ::: UserId -> Handler Response
getSelfH (_ ::: self) = do
    p <- (lift $ API.lookupSelfProfile self) >>= ifNothing userNotFound
    return $ json p

getUserH :: JSON ::: UserId ::: UserId -> Handler Response
getUserH (_ ::: self ::: uid) = do
    p <- (lift $ API.lookupProfile self uid) >>= ifNothing userNotFound
    return $ json p

getUserNameH :: JSON ::: UserId -> Handler Response
getUserNameH (_ ::: self) = do
    name <- lift $ API.lookupName self
    return $ case name of
        Just n  -> json $ object ["name" .= n]
        Nothing -> setStatus status404 empty

listUsersH :: JSON ::: UserId ::: Either (List UserId) (List Handle) -> Handler Response
listUsersH (_ ::: self ::: qry) = case qry of
    Left  us -> toResponse =<< byIds (fromList us)
    Right hs -> do
        us <- catMaybes <$> mapM (lift . API.lookupHandle) (fromList hs)
        sameTeamSearchOnly <- fromMaybe False <$> view (settings . searchSameTeamOnly)
        toResponse =<< if sameTeamSearchOnly
            then sameTeamOnly =<< byIds us
            else byIds us
  where
    sameTeamOnly :: [UserProfile] -> Handler [UserProfile]
    sameTeamOnly us = do
        selfTeam <- lift $ Data.lookupUserTeam self
        return $ case selfTeam of
            Just team -> filter (\x -> profileTeam x == Just team) us
            Nothing   -> us

    byIds uids = lift $ API.lookupProfiles self uids

    toResponse profiles = do
        return $ case profiles of
            [] -> setStatus status404 empty
            ps -> json ps

listActivatedAccountsH :: JSON ::: Either (List UserId) (List Handle) -> Handler Response
listActivatedAccountsH (_ ::: qry) = case qry of
    Left  us -> byIds (fromList us)
    Right hs -> do
        us <- mapM (lift . API.lookupHandle) (fromList hs)
        byIds (catMaybes us)
  where
    byIds uids = json
               . filter (isJust . userIdentity . accountUser)
              <$> (lift $ API.lookupAccounts uids)

listAccountsByIdentityH :: JSON ::: Either Email Phone -> Handler Response
listAccountsByIdentityH (_ ::: emailOrPhone) = lift $ json
    <$> API.lookupAccountsByIdentity emailOrPhone

getActivationCodeH :: JSON ::: Either Email Phone -> Handler Response
getActivationCodeH (_ ::: emailOrPhone) = do
    apair <- lift $ API.lookupActivationCode emailOrPhone
    maybe (throwStd activationKeyNotFound) (return . found) apair
  where
    found (k, c) = json $ object [ "key" .= k, "code" .= c ]

getPasswordResetCodeH :: JSON ::: Either Email Phone -> Handler Response
getPasswordResetCodeH (_ ::: emailOrPhone) = do
    apair <- lift $ API.lookupPasswordResetCode emailOrPhone
    maybe (throwStd invalidPwResetKey) (return . found) apair
  where
    found (k, c) = json $ object [ "key" .= k, "code" .= c ]

updateUserH :: UserId ::: ConnId ::: JsonRequest UserUpdate -> Handler Response
updateUserH (uid ::: conn ::: req) = do
    uu <- parseJsonBody req
    lift $ API.updateUser uid conn uu
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
        Just s  -> json $ object ["status" .= s]
        Nothing -> setStatus status404 empty

changePhoneH :: UserId ::: ConnId ::: JsonRequest PhoneUpdate -> Handler Response
changePhoneH (u ::: _ ::: req) = do
    phone <- puPhone <$> parseJsonBody req
    (adata, pn) <- API.changePhone u phone !>> changePhoneError
    loc   <- lift $ API.lookupLocale u
    let apair = (activationKey adata, activationCode adata)
    lift $ sendActivationSms pn apair loc
    return $ setStatus status202 empty

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
checkHandleH (_ ::: h) = do
    handle <- validateHandle h
    owner <- lift $ API.lookupHandle handle
    if | isJust owner
        -- Handle is taken (=> getHandleInfo will return 200)
        -> return $ setStatus status200 empty
       | API.isBlacklistedHandle handle
        -- Handle is free but cannot be taken
        -> throwE (StdError invalidHandle)
       | otherwise
        -- Handle is free and can be taken
        -> return $ setStatus status404 empty

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
        Just  u -> json (UserHandleInfo u)
        Nothing -> setStatus status404 empty

changeHandleH :: UserId ::: ConnId ::: JsonRequest HandleUpdate -> Handler Response
changeHandleH (u ::: conn ::: req) = do
    HandleUpdate h <- parseJsonBody req
    handle <- validateHandle h
    API.changeHandle u conn handle !>> changeHandleError
    return empty

beginPasswordResetH :: JSON ::: JsonRequest NewPasswordReset -> Handler Response
beginPasswordResetH (_ ::: req) = do
    NewPasswordReset target <- parseJsonBody req
    checkWhitelist target
    (u, pair) <- API.beginPasswordReset target !>> pwResetError
    loc       <- lift $ API.lookupLocale u
    lift $ case target of
        Left  email -> sendPasswordResetMail email pair loc
        Right phone -> sendPasswordResetSms  phone pair loc
    return $ setStatus status201 empty

completePasswordResetH :: JSON ::: JsonRequest CompletePasswordReset -> Handler Response
completePasswordResetH (_ ::: req) = do
    CompletePasswordReset{..} <- parseJsonBody req
    API.completePasswordReset cpwrIdent cpwrCode cpwrPassword !>> pwResetError
    return empty

-- docs/reference/user/activation.md {#RefActivationRequest}
sendActivationCodeH :: JsonRequest SendActivationCode -> Handler Response
sendActivationCodeH req = do
    SendActivationCode{..} <- parseJsonBody req
    checkWhitelist saUserKey
    API.sendActivationCode saUserKey saLocale saCall !>> sendActCodeError
    return empty

changeSelfEmailH :: UserId ::: ConnId ::: JsonRequest EmailUpdate -> Handler Response
changeSelfEmailH (u ::: _ ::: req) = changeEmail u req True

-- Deprecated and to be removed after new versions of brig and galley are
-- deployed. Reason for deprecation: it returns N^2 things (which is not
-- needed), it doesn't scale, and it accepts everything in URL parameters,
-- which doesn't work when the list of users is long.
deprecatedGetConnectionsStatusH :: List UserId ::: Maybe Relation -> Handler Response
deprecatedGetConnectionsStatusH (users ::: flt) = do
    r <- lift $ API.lookupConnectionStatus (fromList users) (fromList users)
    return . json $ maybe r (filterByRelation r) flt
  where
    filterByRelation l rel = filter ((==rel) . csStatus) l

getConnectionsStatusH
    :: JSON ::: JsonRequest ConnectionsStatusRequest ::: Maybe Relation
    -> Handler Response
getConnectionsStatusH (_ ::: req ::: flt) = do
    ConnectionsStatusRequest{csrFrom, csrTo} <- parseJsonBody req
    r <- lift $ API.lookupConnectionStatus csrFrom csrTo
    return . json $ maybe r (filterByRelation r) flt
  where
    filterByRelation l rel = filter ((==rel) . csStatus) l

createConnectionH :: JSON ::: UserId ::: ConnId ::: JsonRequest ConnectionRequest -> Handler Response
createConnectionH (_ ::: self ::: conn ::: req) = do
    cr <- parseJsonBody req
    rs <- API.createConnection self cr conn !>> connError
    return $ case rs of
        ConnectionCreated c -> setStatus status201 $ json c
        ConnectionExists  c -> json c

updateConnectionH :: JSON ::: UserId ::: ConnId ::: UserId ::: JsonRequest ConnectionUpdate -> Handler Response
updateConnectionH (_ ::: self ::: conn ::: other ::: req) = do
    newStatus <- cuStatus <$> parseJsonBody req
    mc <- API.updateConnection self other newStatus (Just conn) !>> connError
    return $ case mc of
        Just  c -> json c
        Nothing -> setStatus status204 empty

listConnectionsH :: JSON ::: UserId ::: Maybe UserId ::: Range 1 500 Int32 -> Handler Response
listConnectionsH (_ ::: uid ::: start ::: size) = json <$>
    lift (API.lookupConnections uid start size)

getConnectionH :: JSON ::: UserId ::: UserId -> Handler Response
getConnectionH (_ ::: uid ::: uid') = lift $ do
    conn <- API.lookupConnection uid uid'
    return $ case conn of
        Just c  -> json c
        Nothing -> setStatus status404 empty

revokeIdentityH :: Either Email Phone -> Handler Response
revokeIdentityH emailOrPhone = do
    lift $ API.revokeIdentity emailOrPhone
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
        []      -> setStatus status404 empty
        _       -> json results

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
    case onlyOwner of
       Team.IsOnlyTeamOwnerWithEmail       -> throwStd noOtherOwner
       Team.IsOneOfManyTeamOwnersWithEmail -> pure ()
       Team.IsTeamOwnerWithoutEmail        -> pure ()
       Team.IsNotTeamOwner                 -> pure ()
    return empty

isTeamOwnerH :: UserId ::: TeamId -> Handler Response
isTeamOwnerH (uid ::: tid) = do
    onlyOwner <- lift (Team.teamOwnershipStatus uid tid)
    case onlyOwner of
       Team.IsOnlyTeamOwnerWithEmail       -> pure ()
       Team.IsOneOfManyTeamOwnersWithEmail -> pure ()
       Team.IsTeamOwnerWithoutEmail        -> pure ()
       Team.IsNotTeamOwner                 -> throwStd insufficientTeamPermissions
    return empty

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
    (RichInfoAssocList richInfo) <- normalizeRichInfoAssocList . riuRichInfo <$> parseJsonBody req
    maxSize <- setRichInfoLimit <$> view settings
    when (richInfoAssocListSize richInfo > maxSize) $ throwStd tooLargeRichInfo
    lift $ Data.updateRichInfo uid (RichInfoAssocList richInfo)
    -- FUTUREWORK: send an event
    -- Intra.onUserEvent uid (Just conn) (richInfoUpdate uid ri)
    return empty

deleteUserH :: UserId ::: JsonRequest DeleteUser ::: JSON -> Handler Response
deleteUserH (u ::: r ::: _) = do
    body <- parseJsonBody r
    res <- API.deleteUser u (deleteUserPassword body) !>> deleteUserError
    return $ case res of
        Nothing  -> setStatus status200 empty
        Just ttl -> setStatus status202 (json (DeletionCodeTimeout ttl))

verifyDeleteUserH :: JsonRequest VerifyDeleteUser ::: JSON -> Handler Response
verifyDeleteUserH (r ::: _) = do
    body <- parseJsonBody r
    API.verifyDeleteUser body !>> deleteUserError
    return (setStatus status200 empty)

onboardingH :: JSON ::: UserId ::: JsonRequest AddressBook -> Handler Response
onboardingH (_ ::: uid ::: r) = do
    ab <- parseJsonBody r
    json <$> API.onboarding uid ab !>> connError

getContactListH :: JSON ::: UserId -> Handler Response
getContactListH (_ ::: uid) = do
    contacts <- lift $ API.lookupContactList uid
    return $ json $ (UserIds contacts)

-- activation

-- docs/reference/user/activation.md {#RefActivationSubmit}
activateKeyH :: JSON ::: JsonRequest Activate -> Handler Response
activateKeyH (_ ::: req) = respFromActivationResponseWithStatus <$> (activate =<< parseJsonBody req)

activateH :: ActivationKey ::: ActivationCode -> Handler Response
activateH (k ::: c) = respFromActivationResponseWithStatus <$> (activate (Activate (ActivateKey k) c False))

activate :: Activate -> Handler ActivationResponseWithStatus
activate (Activate tgt code dryrun)
    | dryrun = do
        API.preverify tgt code !>> actError
        return $ ActivationResponseNothing200
    | otherwise = do
        result <- API.activate tgt code Nothing !>> actError
        return $ case result of
            ActivationSuccess ident first -> respond ident first
            ActivationPass                -> ActivationResponseNothing204
  where
    respond (Just ident) first = ActivationResponseJust $ ActivationResponse ident first
    respond Nothing      _     = ActivationResponseNothing200

data ActivationResponseWithStatus
    = ActivationResponseJust ActivationResponse
    | ActivationResponseNothing200
    | ActivationResponseNothing204

respFromActivationResponseWithStatus :: ActivationResponseWithStatus -> Response
respFromActivationResponseWithStatus = \case
    ActivationResponseJust aresp -> json aresp
    ActivationResponseNothing200 -> empty
    ActivationResponseNothing204 -> setStatus status204 empty

-- Deprecated

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
        ChangeEmailIdempotent                       -> respond status204
        ChangeEmailNeedsActivation (usr, adata, en) -> handleActivation usr adata en
  where
    respond = return . flip setStatus empty
    handleActivation usr adata en = do
        when sendOutEmail $ do
            let apair = (activationKey adata, activationCode adata)
            let name  = userName usr
            let ident = userIdentity usr
            let lang  = userLocale usr
            lift $ sendActivationMail en name apair (Just lang) ident
        respond status202

validateHandle :: Text -> Handler Handle
validateHandle = maybe (throwE (StdError invalidHandle)) return . parseHandle

ifNothing :: Utilities.Error -> Maybe a -> Handler a
ifNothing e = maybe (throwStd e) return

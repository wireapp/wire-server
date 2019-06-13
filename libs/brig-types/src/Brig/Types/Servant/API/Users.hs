{-# OPTIONS_GHC -Wno-unused-imports #-}



module Brig.Types.Servant.API.Users where

import Imports

import qualified "swagger" Data.Swagger.Build.Api as Swagger1
import "swagger2" Data.Swagger as Swagger2

import Brig.Types.Activation
import Brig.Types.Client.Prekey (PrekeyId, Prekey, LastPrekey)
import Brig.Types.Connection
import Brig.Types.Intra
import Brig.Types.Provider
import Brig.Types.Servant.Orphans
import Brig.Types.User
import Brig.Types.User.Auth (CookieLabel)
import Control.Lens
import Data.Aeson as Aeson
import Data.ByteString.Conversion (List(..))
import Data.Currency (Alpha)
import Data.HashMap.Strict.InsOrd
import Data.Id
import Data.ISO3166_CountryCodes ()
import Data.LanguageCodes ()
import Data.Misc
import Data.Proxy
import Data.Range
import Data.Text.Ascii
import Data.Text as Text (unlines)
import Data.UUID (UUID, fromText)
import Galley.Types
import Galley.Types.Bot.Service
import Galley.Types.Teams
import qualified Data.Json.Util
import qualified Data.Metrics as Metrics
import qualified Servant
import Servant.API.Generic
import Servant hiding (Get, Put, Post, Delete, ReqBody, QueryParam, QueryParam', URI)
import Servant.Swagger
import URI.ByteString.QQ (uri)
import URI.ByteString (URI)

import GHC.TypeLits


import Data.String.Conversions
import System.Process (system)
import Data.Aeson (encode)
import Test.Hspec (hspec)
-- import Brig.Types.Test.Arbitrary ()

main :: IO ()
main = do
  writeFile "/tmp/x" . cs $ encode (toSwagger api)
  void $ system "cat /tmp/x | json_pp" --  && curl -X POST -d @/tmp/x -H 'Content-Type:application/json' http://online.swagger.io/validator/debug | json_pp"


api :: Proxy (ToServantApi API)
api = genericApi (Proxy :: Proxy API)

data API route = API
  { _getApiDocs :: route :-
         "api-docs" :> QueryParamStrict "base_url" URI :> Get Swagger1.ApiDecl

  , _getUidHead :: route :-
         Summary "Check if a user ID exists"
      :> Capture "uid" UserId
      :> AuthZUser
      :> Head NoContent
     -- handler: checkUserExists

  } deriving (Generic)


{-

    ---

    get "/users/:id" (continue getUser) $
        accept "application" "json"
        .&. header "Z-User"
        .&. capture "id"

    document "GET" "user" $ do
        Doc.summary "Get a user by ID"
        Doc.parameter Doc.Path "id" Doc.bytes' $
            Doc.description "User ID"
        Doc.returns (Doc.ref Doc.user)
        Doc.response 200 "User" Doc.end
        Doc.errorResponse userNotFound

    ---

    post "/users/handles" (continue checkHandles) $
        accept "application" "json"
        .&. header "Z-User"
        .&. jsonRequest @CheckHandles

    document "POST" "checkUserHandles" $ do
        Doc.summary "Check availability of user handles"
        Doc.body (Doc.ref Doc.checkHandles) $
            Doc.description "JSON body"
        Doc.returns (Doc.array Doc.string')
        Doc.response 200 "List of free handles" Doc.end

    head "/users/handles/:handle" (continue checkHandle) $
        header "Z-User"
        .&. capture "handle"

    document "HEAD" "checkUserHandle" $ do
        Doc.summary "Check whether a user handle can be taken"
        Doc.parameter Doc.Path "handle" Doc.bytes' $
            Doc.description "Handle to check"
        Doc.response 200 "Handle is taken" Doc.end
        Doc.errorResponse invalidHandle
        Doc.errorResponse handleNotFound

    get "/users/handles/:handle" (continue getHandleInfo) $
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

    get "/users" (continue listUsers) $
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

    post "/users/prekeys" (continue getMultiPrekeyBundles) $
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

    get "/users/:user/prekeys" (continue getPrekeyBundle) $
        capture "user"
        .&. accept "application" "json"

    document "GET" "getPrekeyBundle" $ do
        Doc.summary "Get a prekey for each client of a user."
        Doc.parameter Doc.Path "user" Doc.bytes' $
            Doc.description "User ID"
        Doc.returns (Doc.ref Doc.prekeyBundle)
        Doc.response 200 "Prekey Bundle" Doc.end

    ---

    get "/users/:user/prekeys/:client" (continue getPrekey) $
        capture "user"
        .&. capture "client"
        .&. accept "application" "json"

    document "GET" "getPrekey" $ do
        Doc.summary "Get a prekey for a specific client of a user."
        Doc.parameter Doc.Path "user" Doc.bytes' $
            Doc.description "User ID"
        Doc.parameter Doc.Path "client" Doc.bytes' $
            Doc.description "Client ID"
        Doc.returns (Doc.ref Doc.clientPrekey)
        Doc.response 200 "Client Prekey" Doc.end

    --

    get "/users/:user/clients" (continue getUserClients) $
        capture "user"
        .&. accept "application" "json"

    document "GET" "getUserClients" $ do
        Doc.summary "Get all of a user's clients."
        Doc.parameter Doc.Path "user" Doc.bytes' $
            Doc.description "User ID"
        Doc.returns (Doc.array (Doc.ref Doc.pubClient))
        Doc.response 200 "List of clients" Doc.end

    --

    get "/users/:user/clients/:client" (continue getUserClient) $
        capture "user"
        .&. capture "client"
        .&. accept "application" "json"

    document "GET" "getUserClient" $ do
        Doc.summary "Get a specific client of a user."
        Doc.parameter Doc.Path "user" Doc.bytes' $
            Doc.description "User ID"
        Doc.parameter Doc.Path "client" Doc.bytes' $
            Doc.description "Client ID"
        Doc.returns (Doc.ref Doc.pubClient)
        Doc.response 200 "Client" Doc.end

    --

    get "/users/:user/rich-info" (continue getRichInfo) $
        header "Z-User"
        .&. capture "user"
        .&. accept "application" "json"

    document "GET" "getRichInfo" $ do
        Doc.summary "Get user's rich info"
        Doc.parameter Doc.Path "user" Doc.bytes' $
            Doc.description "User ID"
        Doc.returns (Doc.ref Doc.richInfo)
        Doc.response 200 "RichInfo" Doc.end
        Doc.errorResponse insufficientTeamPermissions

    -- /self ------------------------------------------------------------------

    -- Profile

    get "/self" (continue getSelf) $
        accept "application" "json"
        .&. header "Z-User"

    document "GET" "self" $ do
        Doc.summary "Get your profile"
        Doc.returns (Doc.ref Doc.self)
        Doc.response 200 "Self profile" Doc.end

    ---

    put "/self" (continue updateUser) $
        header "Z-User"
        .&. header "Z-Connection"
        .&. jsonRequest @UserUpdate

    document "PUT" "updateSelf" $ do
        Doc.summary "Update your profile"
        Doc.body (Doc.ref Doc.userUpdate) $
            Doc.description "JSON body"
        Doc.response 200 "Update successful." Doc.end

    ---

    get "/self/name" (continue getUserName) $
        accept "application" "json"
        .&. header "Z-User"

    document "GET" "selfName" $ do
        Doc.summary "Get your profile name"
        Doc.returns (Doc.ref Doc.userName)
        Doc.response 200 "Profile name found." Doc.end

    ---

    put "/self/email" (continue changeSelfEmail) $
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

    put "/self/phone" (continue changePhone) $
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

    head "/self/password" (continue checkPasswordExists) $
        header "Z-User"

    document "HEAD" "checkPassword" $ do
        Doc.summary "Check that your password is set"
        Doc.response 200 "Password is set." Doc.end
        Doc.response 404 "Password is not set." Doc.end

    ---

    put "/self/password" (continue changePassword) $
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

    put "/self/locale" (continue changeLocale) $
        header "Z-User"
        .&. header "Z-Connection"
        .&. jsonRequest @LocaleUpdate

    document "PUT" "changeLocale" $ do
        Doc.summary "Change your locale"
        Doc.body (Doc.ref Doc.changeLocale) $
            Doc.description "JSON body"
        Doc.response 200 "Locale changed." Doc.end

    --

    put "/self/handle" (continue changeHandle) $
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

    delete "/self/phone" (continue removePhone) $
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

    delete "/self/email" (continue removeEmail) $
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

    delete "/self" (continue deleteUser) $
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

    post "/delete" (continue verifyDeleteUser) $
        jsonRequest @VerifyDeleteUser
        .&. accept "application" "json"

    document "POST" "verifyDeleteUser" $ do
        Doc.summary "Verify account deletion with a code."
        Doc.body (Doc.ref Doc.verifyDelete) $
            Doc.description "JSON body"
        Doc.response 200 "Deletion is initiated." Doc.end
        Doc.errorResponse invalidCode

    ---

    post "/connections" (continue createConnection) $
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

    get "/connections" (continue listConnections) $
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

    put "/connections/:id" (continue updateConnection) $
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

    get "/connections/:id" (continue getConnection) $
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

    post "/clients" (continue addClient) $
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

    put "/clients/:client" (continue updateClient) $
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

    delete "/clients/:client" (continue rmClient) $
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

    get "/clients" (continue listClients) $
        header "Z-User"
        .&. accept "application" "json"

    document "GET" "listClients" $ do
        Doc.summary "List the registered clients."
        Doc.returns (Doc.array (Doc.ref Doc.client))
        Doc.response 200 "List of clients" Doc.end

    ---

    get "/clients/:client" (continue getClient) $
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

    get "/clients/:client/prekeys" (continue listPrekeyIds) $
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

    put "/properties/:key" (continue setProperty) $
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

    delete "/properties/:key" (continue deleteProperty) $
        header "Z-User"
        .&. header "Z-Connection"
        .&. capture "key"

    document "DELETE" "deleteProperty" $ do
        Doc.summary "Delete a property."
        Doc.parameter Doc.Path "key" Doc.string' $
            Doc.description "Property key"
        Doc.response 200 "Property deleted." Doc.end

    ---

    delete "/properties" (continue clearProperties) $
        header "Z-User"
        .&. header "Z-Connection"

    document "DELETE" "clearProperties" $ do
        Doc.summary "Clear all properties."
        Doc.response 200 "Properties cleared." Doc.end

    ---

    get "/properties/:key" (continue getProperty) $
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

    get "/properties" (continue listPropertyKeys) $
        header "Z-User"
        .&. accept "application" "json"

    document "GET" "listPropertyKeys" $ do
        Doc.summary "List all property keys."
        Doc.returns (Doc.array Doc.string')
        Doc.response 200 "List of property keys." Doc.end

    ---

    get "/properties-values" (continue listPropertyKeysAndValues) $
        header "Z-User"
        .&. accept "application" "json"

    document "GET" "listPropertyKeysAndValues" $ do
        Doc.summary "List all properties with key and value."
        Doc.returns (Doc.ref Doc.propertyDictionary)
        Doc.response 200 "Object with properties as attributes." Doc.end

    -- /register, /activate, /password-reset ----------------------------------

    -- docs/reference/user/registration.md {#RefRegistration}
    post "/register" (continue createUser) $
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

    get "/activate" (continue activate') $
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
    post "/activate" (continue activateKey) $
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
    post "/activate/send" (continue sendActivationCode) $
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

    post "/password-reset" (continue beginPasswordReset) $
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

    post "/password-reset/complete" (continue completePasswordReset) $
        accept "application" "json"
        .&. jsonRequest @CompletePasswordReset

    document "POST" "completePasswordReset" $ do
        Doc.summary "Complete a password reset."
        Doc.body (Doc.ref Doc.completePasswordReset) $
            Doc.description "JSON body"
        Doc.response 200 "Password reset successful." Doc.end
        Doc.errorResponse invalidPwResetCode

    ---

    post "/password-reset/:key" (continue deprecatedCompletePasswordReset) $
        accept "application" "json"
        .&. capture "key"
        .&. jsonRequest @PasswordReset

    document "POST" "deprecatedCompletePasswordReset" $ do
        Doc.deprecated
        Doc.summary "Complete a password reset."
        Doc.notes "DEPRECATED: Use 'POST /password-reset/complete'."

    ---

    post "/onboarding/v3" (continue onboarding) $
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

-}

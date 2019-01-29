{-# LANGUAGE OverloadedStrings #-}

module Brig.Types.Swagger where

import Imports
import Data.Swagger
import Data.Swagger.Build.Api
import Galley.Types.Teams (defaultRole)

import qualified Data.Swagger.Model.Api     as Model
import qualified Galley.Types.Swagger       as Galley
import qualified Galley.Types.Teams.Swagger as Galley

brigModels :: [Model]
brigModels =
    [ -- User
      self
    , user
    , userName
    , newUser
    , userUpdate
    , emailUpdate
    , phoneUpdate
    , newPasswordReset
    , completePasswordReset
    , changePassword
    , changeLocale
    , changeHandle
    , asset
    , userHandleInfo
    , checkHandles

      -- User Connections / Invitations
    , connection
    , connectionRequest
    , connectionUpdate
    , connectionList

      -- Account Activation
    , activate
    , sendActivationCode
    , activationResponse
    , accessToken

      -- Account Deletion
    , delete
    , verifyDelete

      -- Login / Authentication
    , sendLoginCode
    , pendingLoginError
    , loginCodeResponse
    , login
    , removeCookies
    , cookie
    , cookieList

      -- Clients
    , newClient
    , updateClient
    , deleteClient
    , client
    , pubClient
    , sigkeys
    , location

      -- Prekeys
    , prekeyBundle
    , clientPrekey
    , prekey

      -- Properties
    , propertyValue
    , propertyDictionary

      -- Onboarding
    , addressBook
    , card
    , match
    , onboardingMatches

      -- Search
    , searchResult
    , searchContact
    , searchableStatus

      -- Team invitations
    , teamInvitation
    , teamInvitationList
    , teamInvitationRequest

      -- TURN
    , rtcConfiguration
    , rtcIceServer

      -- Re-Exports
    , Galley.newBindingTeam
    , Galley.serviceRef
    ]

-------------------------------------------------------------------------------
-- User Models

self :: Model
self = defineModel "Self" $ do
    description "Self Profile"
    property "id" bytes' $
        description "User ID"
    property "name" string' $
        description "Name"
    property "assets" (array (ref asset)) $
        description "Profile assets"
    property "email" string' $ do
        description "Email address"
        optional
    property "phone" string' $ do
        description "E.164 Phone number"
        optional
    property "accent_id" int32' $ do
        description "Accent colour ID"
        optional
    property "locale" string' $
        description "Locale in <ln-cc> format."
    property "handle" string' $ do
        description "Unique handle."
        optional
    property "deleted" bool' $ do
        description "Whether the account has been deleted."
        optional
    property "managed_by" managedBy $ do
        description "What is the primary source of truth for this user; if it's SCIM \
                    \then the profile can't be edited via normal means"
        optional

user :: Model
user = defineModel "User" $ do
    description "User Profile"
    property "id" bytes' $
        description "User ID"
    property "name" string' $
        description "Name"
    property "assets" (array (ref asset)) $
        description "Profile assets"
    property "accent_id" int32' $ do
        description "Accent colour ID"
        optional
    property "deleted" bool' $ do
        description "Whether the account has been deleted."
        optional
    property "service" (ref Galley.serviceRef) $ do
        description "The reference to the owning service, if the user is a 'bot'."
        optional
    property "handle" string' $ do
        description "Unique user handle."
        optional

managedBy :: DataType
managedBy = string $ enum
    [ "wire"
    , "scim"
    ]

assetType :: DataType
assetType = string $ enum
    [ "image"
    ]

assetSize :: DataType
assetSize = string $ enum
    [ "preview"
    , "complete"
    ]

asset :: Model
asset = defineModel "UserAsset" $ do
    description "User profile asset"
    property "key" string' $
        description "The unique asset key"
    property "type" assetType $
        description "The asset type"
    property "size" assetSize $
        description "The asset size / format"

userName :: Model
userName = defineModel "UserName" $ do
    description "User name"
    property "name" string' $
        description "User name"

newUser :: Model
newUser = defineModel "NewUser" $ do
    description "New User Data"
    property "name" string' $
        description "Name (1 - 128 characters)"
    property "email" string' $ do
        description "Email address"
        optional
    property "password" string' $ do
        description "Password (6 - 1024 characters)"
        optional
    property "assets" (array (ref asset)) $ do
        description "Profile assets"
        optional
    property "phone" string' $ do
        description "E.164 phone number"
        optional
    property "accent_id" int32' $ do
        description "Accent colour ID"
        optional
    property "email_code" bytes' $ do
        description "Email activation code"
        optional
    property "phone_code" bytes' $ do
        description "Phone activation code"
        optional
    property "invitation_code" bytes' $ do
        description "Invitation code. Mutually exclusive with team|team_code"
        optional
    property "locale" string' $ do
        description "Locale in <ln-cc> format."
        optional
    property "label" string' $ do
        description "An optional label to associate with the access cookie, \
                    \if one is granted during account creation."
        optional
    property "team_code" string' $ do
        description "Team invitation code. Mutually exclusive with team|invitation_code"
        optional
    property "team" (ref Galley.newBindingTeam) $ do
        description "New team information. Mutually exclusive with team_code|invitation_code"
        optional

userUpdate :: Model
userUpdate = defineModel "UserUpdate" $ do
    description "User Update Data"
    property "name" string' $
        description "Name (1 - 128 characters)"
    property "assets" (array (ref asset)) $ do
        description "Profile assets"
        optional
    property "accent_id" int32' $ do
        description "Accent colour ID"
        optional

emailUpdate :: Model
emailUpdate = defineModel "EmailUpdate" $ do
    description "Email Update Data"
    property "email" string' $
        description "Email"

phoneUpdate :: Model
phoneUpdate = defineModel "PhoneUpdate" $ do
    description "Phone Update Data"
    property "phone" string' $
        description "E.164 phone number"

newPasswordReset :: Model
newPasswordReset = defineModel "NewPasswordReset" $ do
    description "Data to initiate a password reset"
    property "email" string' $ do
        description "Email"
        optional
    property "phone" string' $ do
        description "Phone"
        optional

completePasswordReset :: Model
completePasswordReset = defineModel "CompletePasswordReset" $ do
    description "Data to complete a password reset."
    property "key" string' $ do
        description "An opaque key for a pending password reset."
        optional
    property "email" string' $ do
        description "A known email with a pending password reset."
        optional
    property "phone" string' $ do
        description "A known phone number with a pending password reset."
        optional
    property "code" string' $
        description "Password reset code"
    property "password" string' $
        description "New password (6 - 1024 characters)"

changePassword :: Model
changePassword = defineModel "ChangePassword" $ do
    description "Data to change a password. The old password is required if \
                \a password already exists."
    property "old_password" string' $ do
        description "Old password"
        optional
    property "new_password" string' $
        description "New password (6 - 1024 characters)"

changeLocale :: Model
changeLocale = defineModel "ChangeLocale" $ do
    description "Data to change a locale."
    property "locale" string' $
        description "Locale to be set"

changeHandle :: Model
changeHandle = defineModel "ChangeHandle" $ do
    description "Change the handle."
    property "handle" string' $
        description "Handle to set"

userHandleInfo :: Model
userHandleInfo = defineModel "UserHandleInfo" $ do
    description "User handle info"
    property "user" string' $
        description "ID of the user owning the handle"

checkHandles :: Model
checkHandles = defineModel "CheckHandles" $ do
    description "Check availability of user handles."
    property "handles" (array string') $
        description "The prioritised list of handles to check (up to 50)"
    property "return" int32' $ do
        description "Desired number of free handles to return (1 - 10). Default 1."
        optional

-------------------------------------------------------------------------------
-- Connection Models

relation :: DataType
relation = string $ enum
    [ "accepted"
    , "blocked"
    , "pending"
    , "ignored"
    , "sent"
    , "cancelled"
    ]

connection :: Model
connection = defineModel "Connection" $ do
    description "Directed connection between two users"
    property "from" bytes' $
        description "User ID"
    property "to" bytes' $
        description "User ID"
    property "status" relation $
        description "Relation status"
    property "last_update" dateTime' $
        description "Timestamp of last update"
    property "message" string' $ do
        description "Message"
        optional
    property "conversation" bytes' $ do
        description "Conversation ID"
        optional

connectionUpdate :: Model
connectionUpdate = defineModel "ConnectionUpdate" $ do
    description "Connection update"
    property "status" relation $
        description "New relation status"

connectionRequest :: Model
connectionRequest = defineModel "ConnectionRequest" $ do
    description "Connection request from one user to another"
    property "user" bytes' $
        description "User ID of the user to request a connection with"
    property "name" string' $
        description "Name of the (pending) conversation being initiated (1 - 256 characters)."
    property "message" string' $
        description "The initial message in the request (1 - 256 characters)."

connectionList :: Model
connectionList = defineModel "UserConnectionList" $ do
    description "A list of user connections."
    property "connections" (unique $ array (ref connection)) end
    property "has_more" bool' $
        description "Indicator that the server has more connections than returned."

-------------------------------------------------------------------------------
-- Team invitation Models

role :: DataType
role = Model.Prim $ Model.Primitive
    { Model.primType     = Model.PrimString
    , Model.defaultValue = Just defaultRole
    , Model.enum         = Just [minBound..]
    , Model.minVal       = Just minBound
    , Model.maxVal       = Just maxBound
    }

teamInvitationRequest :: Model
teamInvitationRequest = defineModel "TeamInvitationRequest" $ do
    description "A request to join a team on Wire."
    property "inviter_name" string' $
        description "Name of the inviter (1 - 128 characters)"
    property "email" string' $
        description "Email of the invitee"
    property "locale" string' $ do
        description "Locale to use for the invitation."
        optional
    property "role" role $ do
        description "Role of the invited user"
        optional

-- | This is *not* the swagger model for the 'TeamInvitation' type (which does not exist), but
-- for the use of 'Invitation' under @/teams/{tid}/invitations@.
--
-- TODO: swagger should be replaced by something more type-safe at some point so this will be
-- forcibly resolved and won't happen again.
teamInvitation :: Model
teamInvitation = defineModel "TeamInvitation" $ do
    description "An invitation to join a team on Wire"
    property "team" bytes' $
        description "Team ID of the inviting team"
    property "role" role $ do
        description "Role of the invited user"
        optional
    property "id" bytes' $
        description "UUID used to refer the invitation"
    property "email" string' $
        description "Email of the invitee"
    property "created_at" dateTime' $
        description "Timestamp of invitation creation"
    property "created_by" bytes' $ do
        description "ID of the inviting user"
        optional
    property "name" string' $
        description "Name of the invitee"

teamInvitationList :: Model
teamInvitationList = defineModel "TeamInvitationList" $ do
    description "A list of sent team invitations."
    property "invitations" (unique $ array (ref teamInvitation)) end
    property "has_more" bool' $
        description "Indicator that the server has more invitations than returned."

-------------------------------------------------------------------------------
-- Activation Models

activate :: Model
activate = defineModel "Activate" $ do
    description "Data for an activation request."
    property "key" string' $ do
        description "An opaque key to activate, as it was sent by the API."
        optional
    property "email" string' $ do
        description "A known email address to activate."
        optional
    property "phone" string' $ do
        description "A known phone number to activate."
        optional
    property "code" string' $
        description "The activation code."
    property "label" string' $ do
        description "An optional label to associate with the access cookie, \
                    \if one is granted during account activation."
        optional
    property "dryrun" bool' $ do
        description "Whether to perform a dryrun, i.e. to only check whether \
                    \activation would succeed. Dry-runs never issue access \
                    \cookies or tokens on success but failures still count \
                    \towards the maximum failure count."
        optional

sendActivationCode :: Model
sendActivationCode = defineModel "SendActivationCode" $ do
    description "Data for requesting an email or phone activation code to be sent. \
                \One of 'email' or 'phone' must be present."
    property "email" string' $ do
        description "Email address to send the code to."
        optional
    property "phone" string' $ do
        description "E.164 phone number to send the code to."
        optional
    property "locale" string' $ do
        description "Locale to use for the activation code template."
        optional
    property "voice_call" bool' $ do
        description "Request the code with a call instead (default is SMS)."
        optional

activationResponse :: Model
activationResponse = defineModel "ActivationResponse" $ do
    description "Response body of a successful activation request"
    property "email" string' $ do
        description "The email address that was activated."
        optional
    property "phone" string' $ do
        description "The phone number that was activated."
        optional
    property "first" bool' $
        description "Whether this is the first successful activation (i.e. account activation)."

-------------------------------------------------------------------------------
-- Deletion Models

delete :: Model
delete = defineModel "Delete" $ do
    description "Data for an account deletion request."
    property "password" string' $ do
        description "The account password to authorise the deletion."
        optional

verifyDelete :: Model
verifyDelete = defineModel "VerifyDelete" $ do
    description "Data for verifying an account deletion."
    property "key" string' $
        description "The identifying key of the account (i.e. user ID)."
    property "code" string' $
        description "The verification code."

-------------------------------------------------------------------------------
-- Login / Authentication Models

sendLoginCode :: Model
sendLoginCode = defineModel "SendLoginCode" $ do
    description "Payload for requesting a login code to be sent."
    property "phone" string' $
        description "E.164 phone number to send the code to."
    property "voice_call" bool' $ do
        description "Request the code with a call instead (default is SMS)."
        optional

pendingLoginError :: Model
pendingLoginError = defineModel "PendingLoginError" $ do
    description "A login code is still pending."
    errorProperties
    property "expires_in" int32' $
        description "Number of seconds before the pending login code expires."

loginCodeResponse :: Model
loginCodeResponse = defineModel "LoginCodeResponse" $ do
    description "A response for a successfully sent login code."
    property "expires_in" int32' $
        description "Number of seconds before the login code expires."

login :: Model
login = defineModel "Login" $ do
    description "Payload for performing a login."
    property "email" string' $ do
        description "The email address for a password login."
        optional
    property "phone" string' $ do
        description "The phone number for a password or SMS login."
        optional
    property "handle" string' $ do
        description "The handle for a password login."
        optional
    property "password" string' $ do
        description "The password for a password login."
        optional
    property "code" string' $ do
        description "The login code for an SMS login."
        optional
    property "label" string' $ do
        description "A label to associate with the returned cookie. \
                    \Every client should have a unique and stable (persistent) label \
                    \to allow targeted revocation of all cookies granted to that \
                    \specific client."
        optional

accessToken :: Model
accessToken = defineModel "AccessToken" $ do
    description "An API access token."
    property "access_token" bytes' $
        description "The opaque access token string."
    property "token_type" (string $ enum ["Bearer"]) $
        description "The type of the access token."
    property "expires_in" int64' $
        description "The number of seconds this token is valid."

removeCookies :: Model
removeCookies = defineModel "RemoveCookies" $ do
    description "Data required to remove cookies"
    property "password" bytes' $
        description "The user's password"
    property "labels" (array bytes') $ do
        description "A list of cookie labels for which to revoke the cookies."
        optional
    property "ids" (array int32') $ do
        description "A list of cookie IDs to revoke."
        optional

cookieType :: DataType
cookieType = string $ enum
    [ "session"
    , "persistent"
    ]

cookie :: Model
cookie = defineModel "Cookie" $ do
    description "Cookie information"
    property "id" int32' $
        description "The primary cookie identifier"
    property "type" cookieType $
        description "The cookie's type"
    property "created" dateTime' $
        description "The cookie's creation time"
    property "expires" dateTime' $
        description "The cookie's expiration time"
    property "label" bytes' $
        description "The cookie's label"

cookieList :: Model
cookieList = defineModel "CookieList" $ do
    description "List of cookie information"
    property "cookies" (array (ref cookie)) end

-----------------------------------------------------------------------------
-- Client Models

clientType :: DataType
clientType = string $ enum
    [ "permanent"
    , "temporary"
    ]

clientClass :: DataType
clientClass = string $ enum
    [ "phone"
    , "tablet"
    , "desktop"
    ]

newClient :: Model
newClient = defineModel "NewClient" $ do
    description "The registration data for a new client."
    property "type" clientType $
        description "The type of client to register. A user may have no more than \
                    \7 (seven) permanent clients and 1 (one) temporary client. When the \
                    \limit of permanent clients is reached, an error is returned. \
                    \When a temporary client already exists, it is replaced."
    property "password" string' $ do
        description "The password of the authenticated user for verification. \
                    \Note: Required for registration of the 2nd, 3rd, ... client."
        optional
    property "prekeys" (array (ref prekey)) $
        description "Prekeys for other clients to establish OTR sessions."
    property "lastkey" (ref prekey) $
        description "The last resort prekey for other clients to establish OTR sessions. \
                    \This key must have the ID 0xFFFF and is never deleted."
    property "sigkeys" (ref sigkeys) $
        description "The signaling keys to use for encryption and signing of OTR native push \
                    \notifications (APNS, GCM)."
    property "label" string' $ do
        description "An optional label to associate with the client."
        optional
    property "class" clientClass $
        description "The device class this client belongs to. Either 'phone', 'tablet', or 'desktop'."
    property "cookie" string' $
        description "The cookie label, i.e. the label used when logging in."
    property "model" string' $ do
        description "Optional model information of this client"
        optional

updateClient :: Model
updateClient = defineModel "UpdateClient" $ do
    description "The new data for the registered client."
    property "prekeys" (array (ref prekey)) $ do
        description "New prekeys for other clients to establish OTR sessions."
        optional
    property "lastkey" (ref prekey) $ do
        description "New last-resort prekey."
        optional
    property "sigkeys" (ref sigkeys) $ do
        description "New signaling keys to use for encryption and signing of OTR native push \
                    \notifications (APNS, GCM)."
        optional
    property "label" string' $ do
        description "A new name for this client."
        optional

deleteClient :: Model
deleteClient = defineModel "DeleteClient" $ do
    description "Required information for client deletion."
    property "password" string' $ do
        description "The password of the authenticated user for verification. \
                    \The password is not required for deleting temporary clients."
        optional

client :: Model
client = defineModel "Client" $ do
    description "A registered client."
    property "type" clientType $
        description "The client type."
    property "id" string' $
        description "The client ID."
    property "label" string' $ do
        description "An optional label associated with the client."
        optional
    property "time" dateTime' $
        description "The date and time when this client was registered."
    property "class" clientClass $
        description "The device class this client belongs to. Either 'phone', 'tablet', or 'desktop'."
    property "cookie" string' $
        description "The cookie label of this client."
    property "address" string' $ do
        description "IP address from which this client has been registered"
        optional
    property "location" (ref location) $ do
        description "Location from which this client has been registered."
        optional
    property "model" string' $ do
        description "Optional model information of this client"
        optional

pubClient :: Model
pubClient = defineModel "PubClient" $ do
    description "A client as seen by other users."
    property "id" string' $
        description "The client ID."
    property "class" clientClass $
        description "The device class this client belongs to. Either 'phone', 'tablet', or 'desktop'."

sigkeys :: Model
sigkeys = defineModel "SignalingKeys" $ do
    description "Signaling keys for encryption and signing of native push notifications (APNS, GCM)."
    property "enckey" bytes' $
        description "The base64-encoded, 256 bit encryption key."
    property "mackey" bytes' $
        description "The base64-encoded, 256 bit MAC key."

location :: Model
location = defineModel "Location" $ do
    description "Geographical location"
    property "lat" double' $
        description "Latitude"
    property "lon" double' $
        description "Longitude"

-----------------------------------------------------------------------------
-- Prekey Models

prekeyBundle :: Model
prekeyBundle = defineModel "PrekeyBundle" $ do
    description "Prekeys of all clients of a single user"
    property "user" bytes' $
        description "User ID"
    property "clients" (array (ref clientPrekey)) $
        description "Prekeys of all clients"

clientPrekey :: Model
clientPrekey = defineModel "ClientPrekey" $ do
    description "Prekey of a single client"
    property "client" bytes' $
        description "Client Id"
    property "prekey" (ref prekey) $
        description "Prekey"

prekey :: Model
prekey = defineModel "Prekey" $ do
    description "Prekey"
    property "id" int32' $
        description "Prekey ID"
    property "key" bytes' $
        description "Prekey data"

-----------------------------------------------------------------------------
-- Properties

propertyValue :: Model
propertyValue = defineModel "PropertyValue" $
    description "A property value is any valid JSON value."

propertyDictionary :: Model
propertyDictionary = defineModel "PropertyDictionary" $
    description "A JSON object with properties as attribute/value pairs."

-----------------------------------------------------------------------------
-- Onboarding

addressBook :: Model
addressBook = defineModel "AddressBook" $ do
    description "Address book of a user"
    property "cards" (array (ref card)) $
        description "List of cards"

card :: Model
card = defineModel "Card" $ do
    description "A contact's card"
    property "contact" (array string') $
        description "List of base64-encoded SHA-256 of a normalised \
                    \email address or phone number"
    property "card_id" string' $ do
        description "Unique card identifier, defined by clients."
        optional

match :: Model
match = defineModel "Match" $ do
    description "A user that got auto-connected as a result of the upload."
    property "id" string' $
        description "Matched user ID"
    property "card_id" string' $ do
        description "DEPRECATED! Use cards instead."
        optional
    property "cards" (array string') $
        description "List of card ids for this match."

onboardingMatches :: Model
onboardingMatches = defineModel "onboardingMatches" $ do
    description "Result of the address book matching"
    property "results" (array (ref match)) $
        description "List of matches."
    property "auto-connects" (array (ref match)) $
        description "List of user IDs matched. It's a bit redudant given 'results' \
                    \but it is here for reasons of backwards compatibility."

--------------------------------------------------------------------------------
-- Search

searchResult :: Model
searchResult = defineModel "SearchResult" $ do
    description "Search Result"
    property "found" int32' $
        description "Total number of hits"
    property "returned" int32' $
        description "Number of hits returned"
    property "took" int32' $
        description "Search time in ms"
    property "documents" (array (ref searchContact)) $
        description "List of contacts found"

searchContact :: Model
searchContact = defineModel "Contact" $ do
    description "Contact discovered through search"
    property "id" string' $
        description "User ID"
    property "name" string' $
        description "Name"
    property "handle" string' $
        description "Handle"
    property "accent_id" int32' $ do
        description "Accent color"
        optional

searchableStatus :: Model
searchableStatus = defineModel "SearchableStatus" $ do
    description "Whether the user is discoverable via search"
    property "searchable" bool' $
        description "'true' if discoverable, 'false' otherwise"

--------------------------------------------------------------------------------
-- TURN

rtcConfiguration :: Model
rtcConfiguration = defineModel "RTCConfiguration" $ do
    description "A subset of the WebRTC 'RTCConfiguration' dictionary"
    property "ice_servers" (array (ref rtcIceServer)) $
        description "Array of 'RTCIceServer' objects"
    property "ttl" int32' $
        description "Number of seconds after which the configuration should be refreshed (advisory)"

rtcIceServer :: Model
rtcIceServer = defineModel "RTCIceServer" $ do
    description "A subset of the WebRTC 'RTCIceServer' object"
    property "urls" (array string') $
        description "Array of TURN server addresses of the form 'turn:<addr>:<port>'"
    property "username" string' $
        description "Username to use for authenticating against the given TURN servers"
    property "credential" string' $
        description "Password to use for authenticating against the given TURN servers"

{-# LANGUAGE OverloadedStrings #-}

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

module Brig.Types.Swagger where

import qualified Data.Swagger.Build.Api as Doc
import Imports
import Wire.API.Provider.Service (modelServiceRef)
import Wire.API.Team (modelNewBindingTeam)
import Wire.API.User.Client.Prekey (modelPrekey)
import Wire.Swagger

brigModels :: [Doc.Model]
brigModels =
  [ -- User
    modelSelf,
    modelUser,
    modelUserDisplayName,
    modelNewUser,
    modelUserUpdate,
    modelEmailUpdate,
    modelPhoneUpdate,
    modelNewPasswordReset,
    modelCompletePasswordReset,
    modelChangePassword,
    modelChangeLocale,
    modelChangeHandle,
    modelAsset,
    modelUserHandleInfo,
    modelCheckHandles,
    modelRichInfo,
    modelRichField,
    -- Account Activation
    modelActivate,
    modelSendActivationCode,
    modelActivationResponse,
    modelAccessToken,
    -- Account Deletion
    modelDelete,
    modelVerifyDelete,
    -- Login / Authentication
    modelPendingLoginError, -- TODO: couldn't find a corresponding type
    -- Clients
    modelNewClient,
    modelUpdateClient,
    modelDeleteClient,
    modelClient,
    modelSigkeys,
    modelLocation,
    modelPubClient,
    -- Properties
    modelPropertyValue,
    modelPropertyDictionary,
    -- Search
    modelSearchResult,
    modelSearchContact
  ]

-------------------------------------------------------------------------------
-- User Models

modelSelf :: Doc.Model
modelSelf = Doc.defineModel "Self" $ do
  Doc.description "Self Profile"
  Doc.property "id" Doc.bytes' $
    Doc.description "User ID"
  Doc.property "name" Doc.string' $
    Doc.description "Name"
  Doc.property "assets" (Doc.array (Doc.ref modelAsset)) $
    Doc.description "Profile assets"
  Doc.property "email" Doc.string' $ do
    Doc.description "Email address"
    Doc.optional
  Doc.property "phone" Doc.string' $ do
    Doc.description "E.164 Phone number"
    Doc.optional
  Doc.property "accent_id" Doc.int32' $ do
    Doc.description "Accent colour ID"
    Doc.optional
  Doc.property "locale" Doc.string' $
    Doc.description "Locale in <ln-cc> format."
  Doc.property "handle" Doc.string' $ do
    Doc.description "Unique handle."
    Doc.optional
  Doc.property "deleted" Doc.bool' $ do
    Doc.description "Whether the account has been deleted."
    Doc.optional
  Doc.property "managed_by" typeManagedBy $ do
    Doc.description
      "What is the source of truth for this user; if it's SCIM \
      \then the profile can't be edited via normal means"
    Doc.optional

modelUser :: Doc.Model
modelUser = Doc.defineModel "User" $ do
  Doc.description "User Profile"
  Doc.property "id" Doc.bytes' $
    Doc.description "User ID"
  Doc.property "name" Doc.string' $
    Doc.description "Name"
  Doc.property "email" Doc.string' $ do
    Doc.description "Email"
    Doc.optional
  Doc.property "assets" (Doc.array (Doc.ref modelAsset)) $
    Doc.description "Profile assets"
  Doc.property "accent_id" Doc.int32' $ do
    Doc.description "Accent colour ID"
    Doc.optional
  Doc.property "deleted" Doc.bool' $ do
    Doc.description "Whether the account has been deleted."
    Doc.optional
  Doc.property "service" (Doc.ref modelServiceRef) $ do
    Doc.description "The reference to the owning service, if the user is a 'bot'."
    Doc.optional
  Doc.property "handle" Doc.string' $ do
    Doc.description "Unique user handle."
    Doc.optional
  Doc.property "team" Doc.string' $ do
    Doc.description "Team ID"
    Doc.optional

typeManagedBy :: Doc.DataType
typeManagedBy =
  Doc.string $
    Doc.enum
      [ "wire",
        "scim"
      ]

typeAssetType :: Doc.DataType
typeAssetType =
  Doc.string $
    Doc.enum
      [ "image"
      ]

typeAssetSize :: Doc.DataType
typeAssetSize =
  Doc.string $
    Doc.enum
      [ "preview",
        "complete"
      ]

modelAsset :: Doc.Model
modelAsset = Doc.defineModel "UserAsset" $ do
  Doc.description "User profile asset"
  Doc.property "key" Doc.string' $
    Doc.description "The unique asset key"
  Doc.property "type" typeAssetType $
    Doc.description "The asset type"
  Doc.property "size" typeAssetSize $
    Doc.description "The asset size / format"

modelRichField :: Doc.Model
modelRichField = Doc.defineModel "RichField" $ do
  Doc.description "RichInfo field"
  Doc.property "type" Doc.string' $
    Doc.description "Field name"
  Doc.property "value" Doc.string' $
    Doc.description "Field value"

modelRichInfo :: Doc.Model
modelRichInfo = Doc.defineModel "RichInfo" $ do
  Doc.description "Rich info about the user"
  Doc.property "fields" (Doc.array (Doc.ref modelRichField)) $
    Doc.description "List of fields"
  Doc.property "version" Doc.int32' $
    Doc.description "Format version (the current version is 0)"

modelUserDisplayName :: Doc.Model
modelUserDisplayName = Doc.defineModel "UserDisplayName" $ do
  Doc.description "User name"
  Doc.property "name" Doc.string' $
    Doc.description "User name"

modelNewUser :: Doc.Model
modelNewUser = Doc.defineModel "NewUser" $ do
  Doc.description "New User Data"
  Doc.property "name" Doc.string' $
    Doc.description "Name (1 - 128 characters)"
  Doc.property "email" Doc.string' $ do
    Doc.description "Email address"
    Doc.optional
  Doc.property "password" Doc.string' $ do
    Doc.description "Password (6 - 1024 characters)"
    Doc.optional
  Doc.property "assets" (Doc.array (Doc.ref modelAsset)) $ do
    Doc.description "Profile assets"
    Doc.optional
  Doc.property "phone" Doc.string' $ do
    Doc.description "E.164 phone number"
    Doc.optional
  Doc.property "accent_id" Doc.int32' $ do
    Doc.description "Accent colour ID"
    Doc.optional
  Doc.property "email_code" Doc.bytes' $ do
    Doc.description "Email activation code"
    Doc.optional
  Doc.property "phone_code" Doc.bytes' $ do
    Doc.description "Phone activation code"
    Doc.optional
  Doc.property "invitation_code" Doc.bytes' $ do
    Doc.description "Invitation code. Mutually exclusive with team|team_code"
    Doc.optional
  Doc.property "locale" Doc.string' $ do
    Doc.description "Locale in <ln-cc> format."
    Doc.optional
  Doc.property "label" Doc.string' $ do
    Doc.description
      "An optional label to associate with the access cookie, \
      \if one is granted during account creation."
    Doc.optional
  Doc.property "team_code" Doc.string' $ do
    Doc.description "Team invitation code. Mutually exclusive with team|invitation_code"
    Doc.optional
  Doc.property "team" (Doc.ref modelNewBindingTeam) $ do
    Doc.description "New team information. Mutually exclusive with team_code|invitation_code"
    Doc.optional

modelUserUpdate :: Doc.Model
modelUserUpdate = Doc.defineModel "UserUpdate" $ do
  Doc.description "User Update Data"
  Doc.property "name" Doc.string' $
    Doc.description "Name (1 - 128 characters)"
  Doc.property "assets" (Doc.array (Doc.ref modelAsset)) $ do
    Doc.description "Profile assets"
    Doc.optional
  Doc.property "accent_id" Doc.int32' $ do
    Doc.description "Accent colour ID"
    Doc.optional

modelEmailUpdate :: Doc.Model
modelEmailUpdate = Doc.defineModel "EmailUpdate" $ do
  Doc.description "Email Update Data"
  Doc.property "email" Doc.string' $
    Doc.description "Email"

modelPhoneUpdate :: Doc.Model
modelPhoneUpdate = Doc.defineModel "PhoneUpdate" $ do
  Doc.description "Phone Update Data"
  Doc.property "phone" Doc.string' $
    Doc.description "E.164 phone number"

modelNewPasswordReset :: Doc.Model
modelNewPasswordReset = Doc.defineModel "NewPasswordReset" $ do
  Doc.description "Data to initiate a password reset"
  Doc.property "email" Doc.string' $ do
    Doc.description "Email"
    Doc.optional
  Doc.property "phone" Doc.string' $ do
    Doc.description "Phone"
    Doc.optional

modelCompletePasswordReset :: Doc.Model
modelCompletePasswordReset = Doc.defineModel "CompletePasswordReset" $ do
  Doc.description "Data to complete a password reset."
  Doc.property "key" Doc.string' $ do
    Doc.description "An opaque key for a pending password reset."
    Doc.optional
  Doc.property "email" Doc.string' $ do
    Doc.description "A known email with a pending password reset."
    Doc.optional
  Doc.property "phone" Doc.string' $ do
    Doc.description "A known phone number with a pending password reset."
    Doc.optional
  Doc.property "code" Doc.string' $
    Doc.description "Password reset code"
  Doc.property "password" Doc.string' $
    Doc.description "New password (6 - 1024 characters)"

modelChangePassword :: Doc.Model
modelChangePassword = Doc.defineModel "ChangePassword" $ do
  Doc.description
    "Data to change a password. The old password is required if \
    \a password already exists."
  Doc.property "old_password" Doc.string' $ do
    Doc.description "Old password"
    Doc.optional
  Doc.property "new_password" Doc.string' $
    Doc.description "New password (6 - 1024 characters)"

modelChangeLocale :: Doc.Model
modelChangeLocale = Doc.defineModel "ChangeLocale" $ do
  Doc.description "Data to change a locale."
  Doc.property "locale" Doc.string' $
    Doc.description "Locale to be set"

modelChangeHandle :: Doc.Model
modelChangeHandle = Doc.defineModel "ChangeHandle" $ do
  Doc.description "Change the handle."
  Doc.property "handle" Doc.string' $
    Doc.description "Handle to set"

modelUserHandleInfo :: Doc.Model
modelUserHandleInfo = Doc.defineModel "UserHandleInfo" $ do
  Doc.description "User handle info"
  Doc.property "user" Doc.string' $
    Doc.description "ID of the user owning the handle"

modelCheckHandles :: Doc.Model
modelCheckHandles = Doc.defineModel "CheckHandles" $ do
  Doc.description "Check availability of user handles."
  Doc.property "handles" (Doc.array Doc.string') $
    Doc.description "The prioritised list of handles to check (up to 50)"
  Doc.property "return" Doc.int32' $ do
    Doc.description "Desired number of free handles to return (1 - 10). Default 1."
    Doc.optional

-------------------------------------------------------------------------------
-- Activation Models

modelActivate :: Doc.Model
modelActivate = Doc.defineModel "Activate" $ do
  Doc.description "Data for an activation request."
  Doc.property "key" Doc.string' $ do
    Doc.description "An opaque key to activate, as it was sent by the API."
    Doc.optional
  Doc.property "email" Doc.string' $ do
    Doc.description "A known email address to activate."
    Doc.optional
  Doc.property "phone" Doc.string' $ do
    Doc.description "A known phone number to activate."
    Doc.optional
  Doc.property "code" Doc.string' $
    Doc.description "The activation code."
  Doc.property "label" Doc.string' $ do
    Doc.description
      "An optional label to associate with the access cookie, \
      \if one is granted during account activation."
    Doc.optional
  Doc.property "dryrun" Doc.bool' $ do
    Doc.description
      "Whether to perform a dryrun, i.e. to only check whether \
      \activation would succeed. Dry-runs never issue access \
      \cookies or tokens on success but failures still count \
      \towards the maximum failure count."
    Doc.optional

modelSendActivationCode :: Doc.Model
modelSendActivationCode = Doc.defineModel "SendActivationCode" $ do
  Doc.description
    "Data for requesting an email or phone activation code to be sent. \
    \One of 'email' or 'phone' must be present."
  Doc.property "email" Doc.string' $ do
    Doc.description "Email address to send the code to."
    Doc.optional
  Doc.property "phone" Doc.string' $ do
    Doc.description "E.164 phone number to send the code to."
    Doc.optional
  Doc.property "locale" Doc.string' $ do
    Doc.description "Locale to use for the activation code template."
    Doc.optional
  Doc.property "voice_call" Doc.bool' $ do
    Doc.description "Request the code with a call instead (default is SMS)."
    Doc.optional

modelActivationResponse :: Doc.Model
modelActivationResponse = Doc.defineModel "ActivationResponse" $ do
  Doc.description "Response body of a successful activation request"
  Doc.property "email" Doc.string' $ do
    Doc.description "The email address that was activated."
    Doc.optional
  Doc.property "phone" Doc.string' $ do
    Doc.description "The phone number that was activated."
    Doc.optional
  Doc.property "first" Doc.bool' $
    Doc.description "Whether this is the first successful activation (i.e. account activation)."

-------------------------------------------------------------------------------
-- Deletion Models

modelDelete :: Doc.Model
modelDelete = Doc.defineModel "Delete" $ do
  Doc.description "Data for an account deletion request."
  Doc.property "password" Doc.string' $ do
    Doc.description "The account password to authorise the deletion."
    Doc.optional

modelVerifyDelete :: Doc.Model
modelVerifyDelete = Doc.defineModel "VerifyDelete" $ do
  Doc.description "Data for verifying an account deletion."
  Doc.property "key" Doc.string' $
    Doc.description "The identifying key of the account (i.e. user ID)."
  Doc.property "code" Doc.string' $
    Doc.description "The verification code."

-------------------------------------------------------------------------------
-- Login / Authentication Models

modelPendingLoginError :: Doc.Model
modelPendingLoginError = Doc.defineModel "PendingLoginError" $ do
  Doc.description "A login code is still pending."
  errorProperties
  Doc.property "expires_in" Doc.int32' $
    Doc.description "Number of seconds before the pending login code expires."

modelAccessToken :: Doc.Model
modelAccessToken = Doc.defineModel "AccessToken" $ do
  Doc.description "An API access token."
  Doc.property "access_token" Doc.bytes' $
    Doc.description "The opaque access token string."
  Doc.property "token_type" (Doc.string $ Doc.enum ["Bearer"]) $
    Doc.description "The type of the access token."
  Doc.property "expires_in" Doc.int64' $
    Doc.description "The number of seconds this token is valid."

-----------------------------------------------------------------------------
-- Client Models

typeClientType :: Doc.DataType
typeClientType =
  Doc.string $
    Doc.enum
      [ "permanent",
        "temporary",
        "legalhold"
      ]

typeClientClass :: Doc.DataType
typeClientClass =
  Doc.string $
    Doc.enum
      [ "phone",
        "tablet",
        "desktop",
        "legalhold"
      ]

modelNewClient :: Doc.Model
modelNewClient = Doc.defineModel "NewClient" $ do
  Doc.description "The registration data for a new client."
  Doc.property "type" typeClientType $
    Doc.description
      "The type of client to register. A user may have no more than \
      \7 (seven) permanent clients and 1 (one) temporary client. When the \
      \limit of permanent clients is reached, an error is returned. \
      \When a temporary client already exists, it is replaced."
  Doc.property "password" Doc.string' $ do
    Doc.description
      "The password of the authenticated user for verification. \
      \Note: Required for registration of the 2nd, 3rd, ... client."
    Doc.optional
  Doc.property "prekeys" (Doc.array (Doc.ref modelPrekey)) $
    Doc.description "Prekeys for other clients to establish OTR sessions."
  Doc.property "lastkey" (Doc.ref modelPrekey) $
    Doc.description
      "The last resort prekey for other clients to establish OTR sessions. \
      \This key must have the ID 0xFFFF and is never deleted."
  Doc.property "sigkeys" (Doc.ref modelSigkeys) $
    Doc.description
      "The signaling keys to use for encryption and signing of OTR native push \
      \notifications (APNS, GCM)."
  Doc.property "label" Doc.string' $ do
    Doc.description "An optional label to associate with the client."
    Doc.optional
  Doc.property "class" typeClientClass $
    Doc.description "The device class this client belongs to. Either 'phone', 'tablet', or 'desktop'."
  Doc.property "cookie" Doc.string' $
    Doc.description "The cookie label, i.e. the label used when logging in."
  Doc.property "model" Doc.string' $ do
    Doc.description "Optional model information of this client"
    Doc.optional

modelUpdateClient :: Doc.Model
modelUpdateClient = Doc.defineModel "UpdateClient" $ do
  Doc.description "The new data for the registered client."
  Doc.property "prekeys" (Doc.array (Doc.ref modelPrekey)) $ do
    Doc.description "New prekeys for other clients to establish OTR sessions."
    Doc.optional
  Doc.property "lastkey" (Doc.ref modelPrekey) $ do
    Doc.description "New last-resort prekey."
    Doc.optional
  Doc.property "sigkeys" (Doc.ref modelSigkeys) $ do
    Doc.description
      "New signaling keys to use for encryption and signing of OTR native push \
      \notifications (APNS, GCM)."
    Doc.optional
  Doc.property "label" Doc.string' $ do
    Doc.description "A new name for this client."
    Doc.optional

modelDeleteClient :: Doc.Model
modelDeleteClient = Doc.defineModel "DeleteClient" $ do
  Doc.description "Required information for client deletion."
  Doc.property "password" Doc.string' $ do
    Doc.description
      "The password of the authenticated user for verification. \
      \The password is not required for deleting temporary clients."
    Doc.optional

modelClient :: Doc.Model
modelClient = Doc.defineModel "Client" $ do
  Doc.description "A registered client."
  Doc.property "type" typeClientType $
    Doc.description "The client type."
  Doc.property "id" Doc.string' $
    Doc.description "The client ID."
  Doc.property "label" Doc.string' $ do
    Doc.description "An optional label associated with the client."
    Doc.optional
  Doc.property "time" Doc.dateTime' $
    Doc.description "The date and time when this client was registered."
  Doc.property "class" typeClientClass $
    Doc.description "The device class this client belongs to."
  Doc.property "cookie" Doc.string' $
    Doc.description "The cookie label of this client."
  Doc.property "address" Doc.string' $ do
    Doc.description "IP address from which this client has been registered"
    Doc.optional
  Doc.property "location" (Doc.ref modelLocation) $ do
    Doc.description "Location from which this client has been registered."
    Doc.optional
  Doc.property "model" Doc.string' $ do
    Doc.description "Optional model information of this client"
    Doc.optional

modelPubClient :: Doc.Model
modelPubClient = Doc.defineModel "PubClient" $ do
  Doc.description "A client as seen by other users."
  Doc.property "id" Doc.string' $
    Doc.description "The client ID."
  Doc.property "class" typeClientClass $
    Doc.description "The device class this client belongs to. Either 'phone', 'tablet', or 'desktop'."

modelSigkeys :: Doc.Model
modelSigkeys = Doc.defineModel "SignalingKeys" $ do
  Doc.description "Signaling keys for encryption and signing of native push notifications (APNS, GCM)."
  Doc.property "enckey" Doc.bytes' $
    Doc.description "The base64-encoded, 256 bit encryption key."
  Doc.property "mackey" Doc.bytes' $
    Doc.description "The base64-encoded, 256 bit MAC key."

modelLocation :: Doc.Model
modelLocation = Doc.defineModel "Location" $ do
  Doc.description "Geographical location"
  Doc.property "lat" Doc.double' $
    Doc.description "Latitude"
  Doc.property "lon" Doc.double' $
    Doc.description "Longitude"

-----------------------------------------------------------------------------
-- Properties

modelPropertyValue :: Doc.Model
modelPropertyValue =
  Doc.defineModel "PropertyValue" $
    Doc.description "A property value is any valid JSON value."

modelPropertyDictionary :: Doc.Model
modelPropertyDictionary =
  Doc.defineModel "PropertyDictionary" $
    Doc.description "A JSON object with properties as attribute/value pairs."

--------------------------------------------------------------------------------
-- Search

modelSearchResult :: Doc.Model
modelSearchResult = Doc.defineModel "SearchResult" $ do
  Doc.description "Search Result"
  Doc.property "found" Doc.int32' $
    Doc.description "Total number of hits"
  Doc.property "returned" Doc.int32' $
    Doc.description "Number of hits returned"
  Doc.property "took" Doc.int32' $
    Doc.description "Search time in ms"
  Doc.property "documents" (Doc.array (Doc.ref modelSearchContact)) $
    Doc.description "List of contacts found"

modelSearchContact :: Doc.Model
modelSearchContact = Doc.defineModel "Contact" $ do
  Doc.description "Contact discovered through search"
  Doc.property "id" Doc.string' $
    Doc.description "User ID"
  Doc.property "name" Doc.string' $
    Doc.description "Name"
  Doc.property "handle" Doc.string' $
    Doc.description "Handle"
  Doc.property "accent_id" Doc.int32' $ do
    Doc.description "Accent color"
    Doc.optional
  Doc.property "team" Doc.string' $ do
    Doc.description "Team ID"
    Doc.optional

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
import Wire.API.User.Profile (modelAsset, typeManagedBy)
import Wire.Swagger

brigModels :: [Doc.Model]
brigModels =
  [ -- User
    modelSelf,
    modelUser,
    modelNewUser,
    modelUserUpdate,
    modelEmailUpdate,
    modelPhoneUpdate,
    modelChangeLocale,
    modelChangeHandle,
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

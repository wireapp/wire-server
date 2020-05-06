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
import Wire.Swagger (errorProperties)

brigModels :: [Doc.Model]
brigModels =
  [ -- Account Activation
    modelAccessToken,
    -- Account Deletion
    modelDelete,
    modelVerifyDelete,
    -- Login / Authentication
    modelPendingLoginError -- TODO: couldn't find a corresponding type
  ]

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

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

module Stern.Swagger where

import Data.ByteString.Conversion (toByteString')
import Data.String.Conversions
import Data.Swagger.Build.Api
import qualified Data.Swagger.Build.Api as Doc
import Imports
import qualified Wire.API.Team.Feature as Feature
import qualified Wire.API.Team.Feature as Public
import Wire.API.Team.SearchVisibility (modelTeamSearchVisibility)

sternModels :: [Model]
sternModels =
  [ emailUpdate,
    phoneUpdate,
    modelTeamSearchVisibility,
    teamBillingInfo,
    teamBillingInfoUpdate
  ]
    <> (Feature.modelForTeamFeature <$> [minBound ..])

typeTeamFeatureNameNoConfig :: Doc.DataType
typeTeamFeatureNameNoConfig =
  Doc.string . Doc.enum $
    cs . toByteString'
      <$> [ Public.TeamFeatureLegalHold,
            Public.TeamFeatureSSO,
            Public.TeamFeatureSearchVisibility,
            Public.TeamFeatureValidateSAMLEmails,
            Public.TeamFeatureDigitalSignatures
          ]

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

teamBillingInfo :: Model
teamBillingInfo = defineModel "teamBillingInfo" $ do
  property "firstname" string' $
    description "First name of the team owner"
  property "lastname" string' $
    description "Last name of the team owner"
  property "street" string' $
    description "Street of the company address"
  property "zip" string' $
    description "ZIP code of the company address"
  property "city" string' $
    description "City of the company address"
  property "country" string' $
    description "Country of the company address"
  property "company" string' $ do
    description "Name of the company"
    optional
  property "state" string' $ do
    description "State of the company address"
    optional

teamBillingInfoUpdate :: Model
teamBillingInfoUpdate = defineModel "teamBillingInfoUpdate" $ do
  property "firstname" string' $ do
    description "First name of the team owner (1 - 256 characters)"
    optional
  property "lastname" string' $ do
    description "Last name of the team owner (1 - 256 characters)"
    optional
  property "street" string' $ do
    description "Street of the company address (1 - 256 characters)"
    optional
  property "zip" string' $ do
    description "ZIP code of the company address (1 - 16 characters)"
    optional
  property "city" string' $ do
    description "City of the company address (1 - 256 characters)"
    optional
  property "country" string' $ do
    description "Country of the company address (1 - 256 characters)"
    optional
  property "company" string' $ do
    description "Name of the company (1 - 256 characters)"
    optional
  property "state" string' $ do
    description "State of the company address (1 - 256 characters)"
    optional

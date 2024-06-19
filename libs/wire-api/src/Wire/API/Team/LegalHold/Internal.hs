{-# LANGUAGE OverloadedStrings #-}

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

module Wire.API.Team.LegalHold.Internal
  ( LegalHoldService (..),
    legalHoldService,
    viewLegalHoldService,
    LegalHoldClientRequest (..),
  )
where

import Data.Aeson
import Data.Id
import Data.Json.Util
import Data.Misc
import Data.OpenApi qualified as Swagger
import Data.Schema qualified as Schema
import Imports
import Wire.API.Provider
import Wire.API.Provider.Service
import Wire.API.Team.LegalHold
import Wire.API.User.Client.Prekey

data LegalHoldService = LegalHoldService
  { legalHoldServiceTeam :: !TeamId,
    legalHoldServiceUrl :: !HttpsUrl,
    legalHoldServiceFingerprint :: !(Fingerprint Rsa),
    legalHoldServiceToken :: !ServiceToken,
    legalHoldServiceKey :: !ServiceKey
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON LegalHoldService where
  toJSON s =
    object $
      "team_id"
        .= legalHoldServiceTeam s
        # "base_url"
        .= legalHoldServiceUrl s
        # "fingerprint"
        .= legalHoldServiceFingerprint s
        # "auth_token"
        .= legalHoldServiceToken s
        # "public_key"
        .= legalHoldServiceKey s
        # []

instance FromJSON LegalHoldService where
  parseJSON = withObject "LegalHoldService" $ \o ->
    LegalHoldService
      <$> o
        .: "team_id"
      <*> o
        .: "base_url"
      <*> o
        .: "fingerprint"
      <*> o
        .: "auth_token"
      <*> o
        .: "public_key"

legalHoldService :: TeamId -> Fingerprint Rsa -> NewLegalHoldService -> ServiceKey -> LegalHoldService
legalHoldService tid fpr (NewLegalHoldService u _ t) = LegalHoldService tid u fpr t

viewLegalHoldService :: LegalHoldService -> ViewLegalHoldService
viewLegalHoldService (LegalHoldService tid u fpr t k) =
  ViewLegalHoldService $ ViewLegalHoldServiceInfo tid u fpr t (serviceKeyPEM k)

-- | This request is used by Galley to notify Brig that a LegalHold client was requested.
data LegalHoldClientRequest = LegalHoldClientRequest
  { lhcrRequester :: !UserId,
    lhcrLastPrekey :: !LastPrekey
  }
  deriving stock (Eq, Show, Generic)
  deriving (Swagger.ToSchema, FromJSON, ToJSON) via Schema.Schema LegalHoldClientRequest

instance Schema.ToSchema LegalHoldClientRequest where
  schema =
    Schema.object
      "LegalHoldClientRequest"
      $ LegalHoldClientRequest
        <$> lhcrRequester Schema..= Schema.field "requester" Schema.schema
        <*> lhcrLastPrekey Schema..= Schema.field "last_prekey" Schema.schema

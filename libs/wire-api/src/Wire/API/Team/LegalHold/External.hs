{-# LANGUAGE StrictData #-}

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

-- | Types used by the Wire server for outbound requests to a LegalHold service.
module Wire.API.Team.LegalHold.External
  ( -- * initiate
    RequestNewLegalHoldClient (..),
    RequestNewLegalHoldClientV1 (..),
    NewLegalHoldClient (..),

    -- * confirm
    LegalHoldServiceConfirm (..),

    -- * remove
    LegalHoldServiceRemove (..),

    -- * SupportedVersions
    SupportedVersions (..),
  )
where

import Data.Aeson hiding (fieldLabelModifier)
import Data.Id
import Data.Json.Util ((#))
import Data.OpenApi
import Data.Qualified
import Data.Schema qualified as Schema
import Imports
import Wire.API.User.Client.Prekey
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

--------------------------------------------------------------------------------
-- initiate

-- | Request payload that the LH service endpoint @/initiate@ expects
data RequestNewLegalHoldClient = RequestNewLegalHoldClient
  { userId :: UserId,
    teamId :: TeamId
  }
  deriving stock (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform RequestNewLegalHoldClient)

instance ToSchema RequestNewLegalHoldClient where
  declareNamedSchema = genericDeclareNamedSchema opts
    where
      opts =
        defaultSchemaOptions
          { fieldLabelModifier = \case
              "userId" -> "user_id"
              "teamId" -> "team_id"
              _ -> ""
          }

instance ToJSON RequestNewLegalHoldClient where
  toJSON (RequestNewLegalHoldClient userId teamId) =
    object $
      "user_id"
        .= userId
        # "team_id"
        .= teamId
        # []

instance FromJSON RequestNewLegalHoldClient where
  parseJSON = withObject "RequestNewLegalHoldClient" $ \o ->
    RequestNewLegalHoldClient
      <$> o .: "user_id"
      <*> o .: "team_id"

data RequestNewLegalHoldClientV1 = RequestNewLegalHoldClientV1
  { userId :: Qualified UserId,
    teamId :: TeamId
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON) via (Schema.Schema RequestNewLegalHoldClientV1)

instance Schema.ToSchema RequestNewLegalHoldClientV1 where
  schema =
    Schema.object "RequestNewLegalHoldClientV1" $
      RequestNewLegalHoldClientV1
        <$> (.userId) Schema..= Schema.field "qualified_user_id" Schema.schema
        <*> (.teamId) Schema..= Schema.field "team_id" Schema.schema

-- | Response payload that the LH service returns upon calling @/initiate@
data NewLegalHoldClient = NewLegalHoldClient
  { newLegalHoldClientPrekeys :: [Prekey],
    newLegalHoldClientLastKey :: LastPrekey
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewLegalHoldClient)

instance ToSchema NewLegalHoldClient where
  declareNamedSchema = genericDeclareNamedSchema opts
    where
      opts =
        defaultSchemaOptions
          { fieldLabelModifier = \case
              "newLegalHoldClientPrekeys" -> "prekeys"
              "newLegalHoldClientLastKey" -> "last_prekey"
              _ -> ""
          }

instance ToJSON NewLegalHoldClient where
  toJSON c =
    object $
      "prekeys"
        .= newLegalHoldClientPrekeys c
        # "last_prekey"
        .= newLegalHoldClientLastKey c
        # []

instance FromJSON NewLegalHoldClient where
  parseJSON = withObject "NewLegalHoldClient" $ \o ->
    NewLegalHoldClient
      <$> o .: "prekeys"
      <*> o .: "last_prekey"

--------------------------------------------------------------------------------
-- confirm

-- Request payload for the @/confirm@ endpoint on the LegalHold Service
data LegalHoldServiceConfirm = LegalHoldServiceConfirm
  { lhcClientId :: ClientId,
    lhcUserId :: UserId,
    lhcTeamId :: TeamId,
    -- | Replace with Legal Hold Token Type
    lhcRefreshToken :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform LegalHoldServiceConfirm)

instance ToJSON LegalHoldServiceConfirm where
  toJSON (LegalHoldServiceConfirm clientId userId teamId refreshToken) =
    object $
      "client_id"
        .= clientId
        # "user_id"
        .= userId
        # "team_id"
        .= teamId
        # "refresh_token"
        .= refreshToken
        # []

instance FromJSON LegalHoldServiceConfirm where
  parseJSON = withObject "LegalHoldServiceConfirm" $ \o ->
    LegalHoldServiceConfirm
      <$> o .: "client_id"
      <*> o .: "user_id"
      <*> o .: "team_id"
      <*> o .: "refresh_token"

--------------------------------------------------------------------------------
-- remove

-- Request payload for the @/remove@ endpoint on the LegalHold Service
data LegalHoldServiceRemove = LegalHoldServiceRemove
  { lhrUserId :: UserId,
    lhrTeamId :: TeamId
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform LegalHoldServiceRemove)

instance ToJSON LegalHoldServiceRemove where
  toJSON (LegalHoldServiceRemove userId teamId) =
    object $
      "user_id"
        .= userId
        # "team_id"
        .= teamId
        # []

instance FromJSON LegalHoldServiceRemove where
  parseJSON = withObject "LegalHoldServiceRemove" $ \o ->
    LegalHoldServiceRemove
      <$> o .: "user_id"
      <*> o .: "team_id"

newtype SupportedVersions = SupportedVersions {supported :: [Int]}
  deriving (FromJSON) via (Schema.Schema SupportedVersions)

instance Schema.ToSchema SupportedVersions where
  schema =
    Schema.object "SupportedVersions " $
      SupportedVersions
        <$> supported
          Schema..= Schema.field "supported" (Schema.array Schema.schema)

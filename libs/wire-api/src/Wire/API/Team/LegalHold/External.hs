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
    RequestNewLegalHoldClientV0 (..),
    RequestNewLegalHoldClient (..),
    NewLegalHoldClient (..),

    -- * confirm
    LegalHoldServiceConfirm (..),

    -- * remove
    LegalHoldServiceRemove (..),

    -- * SupportedVersions
    SupportedVersions (..),
  )
where

import Data.Aeson qualified as A hiding (fieldLabelModifier)
import Data.Id
import Data.Json.Util ((#))
import Data.OpenApi qualified as OpenApi
import Data.Qualified
import Data.Schema
import Imports
import Wire.API.User.Client.Prekey
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

--------------------------------------------------------------------------------
-- initiate

-- | Request payload that the LH service endpoint @/initiate@ expects
data RequestNewLegalHoldClientV0 = RequestNewLegalHoldClientV0
  { userId :: UserId,
    teamId :: TeamId
  }
  deriving stock (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform RequestNewLegalHoldClientV0)
  deriving (A.ToJSON, A.FromJSON) via (Schema RequestNewLegalHoldClientV0)

instance ToSchema RequestNewLegalHoldClientV0 where
  schema =
    object "RequestNewLegalHoldClientV0" $
      RequestNewLegalHoldClientV0
        <$> (.userId) .= field "user_id" schema
        <*> (.teamId) .= field "team_id" schema

data RequestNewLegalHoldClient = RequestNewLegalHoldClient
  { userId :: Qualified UserId,
    teamId :: TeamId
  }
  deriving stock (Show, Eq, Generic)
  deriving (A.ToJSON, A.FromJSON) via (Schema RequestNewLegalHoldClient)
  deriving (Arbitrary) via (GenericUniform RequestNewLegalHoldClient)

instance ToSchema RequestNewLegalHoldClient where
  schema =
    object "RequestNewLegalHoldClient" $
      RequestNewLegalHoldClient
        <$> (.userId) .= field "qualified_user_id" schema
        <*> (.teamId) .= field "team_id" schema

-- | Response payload that the LH service returns upon calling @/initiate@
data NewLegalHoldClient = NewLegalHoldClient
  { newLegalHoldClientPrekeys :: [Prekey],
    newLegalHoldClientLastKey :: LastPrekey
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewLegalHoldClient)

instance OpenApi.ToSchema NewLegalHoldClient where
  declareNamedSchema = OpenApi.genericDeclareNamedSchema opts
    where
      opts =
        OpenApi.defaultSchemaOptions
          { OpenApi.fieldLabelModifier = \case
              "newLegalHoldClientPrekeys" -> "prekeys"
              "newLegalHoldClientLastKey" -> "last_prekey"
              _ -> ""
          }

instance A.ToJSON NewLegalHoldClient where
  toJSON c =
    A.object $
      "prekeys"
        A..= newLegalHoldClientPrekeys c
        # "last_prekey"
        A..= newLegalHoldClientLastKey c
        # []

instance A.FromJSON NewLegalHoldClient where
  parseJSON = A.withObject "NewLegalHoldClient" $ \o ->
    NewLegalHoldClient
      <$> o A..: "prekeys"
      <*> o A..: "last_prekey"

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

instance A.ToJSON LegalHoldServiceConfirm where
  toJSON (LegalHoldServiceConfirm clientId userId teamId refreshToken) =
    A.object $
      "client_id"
        A..= clientId
        # "user_id"
        A..= userId
        # "team_id"
        A..= teamId
        # "refresh_token"
        A..= refreshToken
        # []

instance A.FromJSON LegalHoldServiceConfirm where
  parseJSON = A.withObject "LegalHoldServiceConfirm" $ \o ->
    LegalHoldServiceConfirm
      <$> o A..: "client_id"
      <*> o A..: "user_id"
      <*> o A..: "team_id"
      <*> o A..: "refresh_token"

--------------------------------------------------------------------------------
-- remove

-- Request payload for the @/remove@ endpoint on the LegalHold Service
data LegalHoldServiceRemove = LegalHoldServiceRemove
  { lhrUserId :: UserId,
    lhrTeamId :: TeamId
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform LegalHoldServiceRemove)

instance A.ToJSON LegalHoldServiceRemove where
  toJSON (LegalHoldServiceRemove userId teamId) =
    A.object $
      "user_id"
        A..= userId
        # "team_id"
        A..= teamId
        # []

instance A.FromJSON LegalHoldServiceRemove where
  parseJSON = A.withObject "LegalHoldServiceRemove" $ \o ->
    LegalHoldServiceRemove
      <$> o A..: "user_id"
      <*> o A..: "team_id"

newtype SupportedVersions = SupportedVersions {supported :: [Int]}
  deriving (A.FromJSON) via (Schema SupportedVersions)

instance ToSchema SupportedVersions where
  schema =
    object "SupportedVersions " $
      SupportedVersions
        <$> supported
          .= field "supported" (array schema)

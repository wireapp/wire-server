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
    LegalHoldServiceConfirmV0 (..),
    LegalHoldServiceConfirm (..),

    -- * remove
    LegalHoldServiceRemoveV0 (..),
    LegalHoldServiceRemove (..),

    -- * SupportedVersions
    SupportedVersions (..),
  )
where

import Data.Aeson qualified as A hiding (fieldLabelModifier)
import Data.Id
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
  deriving (A.ToJSON, A.FromJSON) via (Schema NewLegalHoldClient)

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

instance ToSchema NewLegalHoldClient where
  schema =
    object "NewLegalHoldClient" $
      NewLegalHoldClient
        <$> (.newLegalHoldClientPrekeys) .= field "prekeys" (array schema)
        <*> (.newLegalHoldClientLastKey) .= field "last_prekey" schema

--------------------------------------------------------------------------------
-- confirm

-- Request payload for the @/confirm@ endpoint on the LegalHold Service
data LegalHoldServiceConfirm = LegalHoldServiceConfirm
  { clientId :: ClientId,
    userId :: Qualified UserId,
    teamId :: TeamId,
    -- | Replace with Legal Hold Token Type
    refreshToken :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform LegalHoldServiceConfirm)
  deriving (A.ToJSON, A.FromJSON) via (Schema LegalHoldServiceConfirm)

instance ToSchema LegalHoldServiceConfirm where
  schema =
    object "LegalHoldServiceConfirm" $
      LegalHoldServiceConfirm
        <$> (.clientId) .= field "client_id" schema
        <*> (.userId) .= field "qualified_user_id" schema
        <*> (.teamId) .= field "team_id" schema
        <*> (.refreshToken) .= field "refresh_token" schema

data LegalHoldServiceConfirmV0 = LegalHoldServiceConfirmV0
  { lhcClientId :: ClientId,
    lhcUserId :: UserId,
    lhcTeamId :: TeamId,
    -- | Replace with Legal Hold Token Type
    lhcRefreshToken :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform LegalHoldServiceConfirmV0)
  deriving (A.ToJSON, A.FromJSON) via (Schema LegalHoldServiceConfirmV0)

instance ToSchema LegalHoldServiceConfirmV0 where
  schema =
    object "LegalHoldServiceConfirmV0" $
      LegalHoldServiceConfirmV0
        <$> (.lhcClientId) .= field "client_id" schema
        <*> (.lhcUserId) .= field "user_id" schema
        <*> (.lhcTeamId) .= field "team_id" schema
        <*> (.lhcRefreshToken) .= field "refresh_token" schema

--------------------------------------------------------------------------------
-- remove

-- Request payload for the @/remove@ endpoint on the LegalHold Service
data LegalHoldServiceRemove = LegalHoldServiceRemove
  { userId :: Qualified UserId,
    teamId :: TeamId
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform LegalHoldServiceRemove)
  deriving (A.ToJSON, A.FromJSON) via (Schema LegalHoldServiceRemove)

instance ToSchema LegalHoldServiceRemove where
  schema =
    object "LegalHoldServiceRemove" $
      LegalHoldServiceRemove
        <$> (.userId) .= field "qualified_user_id" schema
        <*> (.teamId) .= field "team_id" schema

data LegalHoldServiceRemoveV0 = LegalHoldServiceRemoveV0
  { lhrUserId :: UserId,
    lhrTeamId :: TeamId
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform LegalHoldServiceRemoveV0)
  deriving (A.ToJSON, A.FromJSON) via (Schema LegalHoldServiceRemoveV0)

instance ToSchema LegalHoldServiceRemoveV0 where
  schema =
    object "LegalHoldServiceRemoveV0" $
      LegalHoldServiceRemoveV0
        <$> (.lhrUserId) .= field "user_id" schema
        <*> (.lhrTeamId) .= field "team_id" schema

--------------------------------------------------------------------------------
-- SupportedVersions

newtype SupportedVersions = SupportedVersions {supported :: [Int]}
  deriving (A.FromJSON) via (Schema SupportedVersions)

instance ToSchema SupportedVersions where
  schema =
    object "SupportedVersions " $
      SupportedVersions
        <$> supported
          .= field "supported" (array schema)

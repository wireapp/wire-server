{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

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

-- | Types used by the Wire server for outbound requests to a LegalHold service.
module Wire.API.Team.LegalHold.External
  ( -- * initiate
    RequestNewLegalHoldClient (..),
    NewLegalHoldClient (..),

    -- * confirm
    LegalHoldServiceConfirm (..),

    -- * remove
    LegalHoldServiceRemove (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Id
import Data.Schema
import qualified Data.Swagger as S
import Imports
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))
import Wire.API.User.Client.Prekey

--------------------------------------------------------------------------------
-- initiate

-- | Request payload that the LH service endpoint @/initiate@ expects
data RequestNewLegalHoldClient = RequestNewLegalHoldClient
  { userId :: UserId,
    teamId :: TeamId
  }
  deriving stock (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform RequestNewLegalHoldClient)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema RequestNewLegalHoldClient

instance ToSchema RequestNewLegalHoldClient where
  schema =
    object "RequestNewLegalHoldClient" $
      RequestNewLegalHoldClient
        <$> userId .= field "user_id" schema
        <*> teamId .= field "team_id" schema

-- | Response payload that the LH service returns upon calling @/initiate@
data NewLegalHoldClient = NewLegalHoldClient
  { newLegalHoldClientPrekeys :: [Prekey],
    newLegalHoldClientLastKey :: LastPrekey
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewLegalHoldClient)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema NewLegalHoldClient

instance ToSchema NewLegalHoldClient where
  schema =
    object "NewLegalHoldClient" $
      NewLegalHoldClient
        <$> newLegalHoldClientPrekeys .= field "prekeys" (array schema)
        <*> newLegalHoldClientLastKey .= field "last_prekey" schema

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
  deriving (FromJSON, ToJSON) via Schema LegalHoldServiceConfirm

instance ToSchema LegalHoldServiceConfirm where
  schema =
    object "LegalHoldServiceConfirm" $
      LegalHoldServiceConfirm
        <$> lhcClientId .= field "client_id" schema
        <*> lhcUserId .= field "user_id" schema
        <*> lhcTeamId .= field "team_id" schema
        <*> lhcRefreshToken .= field "refresh_token" schema

--------------------------------------------------------------------------------
-- remove

-- Request payload for the @/remove@ endpoint on the LegalHold Service
data LegalHoldServiceRemove = LegalHoldServiceRemove
  { lhrUserId :: UserId,
    lhrTeamId :: TeamId
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform LegalHoldServiceRemove)
  deriving (FromJSON, ToJSON) via Schema LegalHoldServiceRemove

instance ToSchema LegalHoldServiceRemove where
  schema =
    object "LegalHoldServiceRemove" $
      LegalHoldServiceRemove
        <$> lhrUserId .= field "user_id" schema
        <*> lhrTeamId .= field "team_id" schema

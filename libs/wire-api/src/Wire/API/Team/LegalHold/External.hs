{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

-- | TODO: explain
module Wire.API.Team.LegalHold.External
  ( -- * intiate
    RequestNewLegalHoldClient (..),
    NewLegalHoldClient (..),

    -- * confirm
    LegalHoldServiceConfirm (..),

    -- * remove
    LegalHoldServiceRemove (..),
  )
where

import Data.Aeson
import Data.Id
import Data.Json.Util ((#))
import Imports
import Wire.API.User.Client.Prekey

--------------------------------------------------------------------------------
-- initiate

-- | Request payload that the LH service endpoint @/initiate@ expects
data RequestNewLegalHoldClient = RequestNewLegalHoldClient
  { userId :: !UserId,
    teamId :: !TeamId
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON RequestNewLegalHoldClient where
  toJSON (RequestNewLegalHoldClient userId teamId) =
    object $
      "user_id" .= userId
        # "team_id" .= teamId
        # []

instance FromJSON RequestNewLegalHoldClient where
  parseJSON = withObject "RequestNewLegalHoldClient" $ \o ->
    RequestNewLegalHoldClient <$> o .: "user_id"
      <*> o .: "team_id"

-- | Response payload that the LH service returns upon calling @/initiate@
data NewLegalHoldClient = NewLegalHoldClient
  { newLegalHoldClientPrekeys :: [Prekey],
    newLegalHoldClientLastKey :: !LastPrekey
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON NewLegalHoldClient where
  toJSON c =
    object $
      "prekeys" .= newLegalHoldClientPrekeys c
        # "last_prekey" .= newLegalHoldClientLastKey c
        # []

instance FromJSON NewLegalHoldClient where
  parseJSON = withObject "NewLegalHoldClient" $ \o ->
    NewLegalHoldClient <$> o .: "prekeys"
      <*> o .: "last_prekey"

--------------------------------------------------------------------------------
-- confirm

-- Request payload for the @/confirm@ endpoint on the LegalHold Service
data LegalHoldServiceConfirm = LegalHoldServiceConfirm
  { lhcClientId :: !ClientId,
    lhcUserId :: !UserId,
    lhcTeamId :: !TeamId,
    -- | Replace with Legal Hold Token Type
    lhcRefreshToken :: !Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON LegalHoldServiceConfirm where
  toJSON (LegalHoldServiceConfirm clientId userId teamId refreshToken) =
    object $
      "client_id" .= clientId
        # "user_id" .= userId
        # "team_id" .= teamId
        # "refresh_token" .= refreshToken
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
  { lhrUserId :: !UserId,
    lhrTeamId :: !TeamId
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON LegalHoldServiceRemove where
  toJSON (LegalHoldServiceRemove userId teamId) =
    object $
      "user_id" .= userId
        # "team_id" .= teamId
        # []

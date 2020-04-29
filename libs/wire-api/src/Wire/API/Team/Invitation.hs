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

module Wire.API.Team.Invitation where

import Data.Aeson
import Data.Id
import Data.Json.Util
import Imports
import Wire.API.Team
import Wire.API.User.Identity
import Wire.API.User.Profile

data InvitationRequest = InvitationRequest
  { irEmail :: !Email,
    irName :: !Name,
    irLocale :: !(Maybe Locale),
    irRole :: !(Maybe Role),
    irInviteeName :: !(Maybe Name),
    irPhone :: !(Maybe Phone)
  }
  deriving (Eq, Show)

data Invitation = Invitation
  { inTeam :: !TeamId,
    inRole :: !Role,
    inInvitation :: !InvitationId,
    inIdentity :: !Email,
    inCreatedAt :: !UTCTimeMillis,
    -- | this is always 'Just' for new invitations, but for
    -- migration it is allowed to be 'Nothing'.
    inCreatedBy :: !(Maybe UserId),
    inInviteeName :: !(Maybe Name),
    inPhone :: !(Maybe Phone)
  }
  deriving (Eq, Show)

data InvitationList = InvitationList
  { ilInvitations :: [Invitation],
    ilHasMore :: !Bool
  }
  deriving (Eq, Show)

instance FromJSON InvitationRequest where
  parseJSON = withObject "invitation-request" $ \o ->
    InvitationRequest <$> o .: "email"
      <*> o .: "inviter_name"
      <*> o .:? "locale"
      <*> o .:? "role"
      <*> o .:? "name"
      <*> o .:? "phone"

instance ToJSON InvitationRequest where
  toJSON i =
    object $
      [ "email" .= irEmail i,
        "inviter_name" .= irName i,
        "locale" .= irLocale i,
        "role" .= irRole i,
        "name" .= irInviteeName i,
        "phone" .= irPhone i
      ]

instance FromJSON Invitation where
  parseJSON = withObject "invitation" $ \o ->
    Invitation <$> o .: "team"
      -- clients, when leaving "role" empty, can leave the default role choice to us
      <*> o .:? "role" .!= defaultRole
      <*> o .: "id"
      <*> o .: "email"
      <*> o .: "created_at"
      <*> o .:? "created_by"
      <*> o .:? "name"
      <*> o .:? "phone"

instance ToJSON Invitation where
  toJSON i =
    object $
      [ "team" .= inTeam i,
        "role" .= inRole i,
        "id" .= inInvitation i,
        "email" .= inIdentity i,
        "created_at" .= inCreatedAt i,
        "created_by" .= inCreatedBy i,
        "name" .= inInviteeName i,
        "phone" .= inPhone i
      ]

instance ToJSON InvitationList where
  toJSON (InvitationList l m) =
    object
      [ "invitations" .= l,
        "has_more" .= m
      ]

instance FromJSON InvitationList where
  parseJSON = withObject "InvitationList" $ \o ->
    InvitationList <$> o .: "invitations"
      <*> o .: "has_more"

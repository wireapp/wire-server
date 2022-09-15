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

module Wire.API.Team.Invitation
  ( InvitationRequest (..),
    Invitation (..),
    InvitationList (..),

    -- * Swagger
    modelTeamInvitation,
    modelTeamInvitationList,
    modelTeamInvitationRequest,
  )
where

import Data.Aeson
import Data.Id
import Data.Json.Util
import qualified Data.Swagger.Build.Api as Doc
import Imports
import URI.ByteString
import Wire.API.Team.Role (Role, defaultRole, typeRole)
import Wire.API.User.Identity (Email, Phone)
import Wire.API.User.Profile (Locale, Name)
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

--------------------------------------------------------------------------------
-- InvitationRequest

data InvitationRequest = InvitationRequest
  { irLocale :: Maybe Locale,
    irRole :: Maybe Role,
    irInviteeName :: Maybe Name,
    irInviteeEmail :: Email,
    irInviteePhone :: Maybe Phone
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform InvitationRequest)

modelTeamInvitationRequest :: Doc.Model
modelTeamInvitationRequest = Doc.defineModel "TeamInvitationRequest" $ do
  Doc.description "A request to join a team on Wire."
  Doc.property "locale" Doc.string' $ do
    Doc.description "Locale to use for the invitation."
    Doc.optional
  Doc.property "role" typeRole $ do
    Doc.description "Role of the invitee (invited user)."
    Doc.optional
  Doc.property "name" Doc.string' $ do
    Doc.description "Name of the invitee (1 - 128 characters)."
    Doc.optional
  Doc.property "email" Doc.string' $
    Doc.description "Email of the invitee."
  Doc.property "phone" Doc.string' $ do
    Doc.description "Phone number of the invitee, in the E.164 format."
    Doc.optional
  Doc.property "inviter_name" Doc.string' $
    Doc.description "DEPRECATED - WILL BE IGNORED IN FAVOR OF REQ AUTH DATA - Name of the inviter (1 - 128 characters)."

instance ToJSON InvitationRequest where
  toJSON i =
    object
      [ "locale" .= irLocale i,
        "role" .= irRole i,
        "name" .= irInviteeName i,
        "email" .= irInviteeEmail i,
        "phone" .= irInviteePhone i
      ]

instance FromJSON InvitationRequest where
  parseJSON = withObject "invitation-request" $ \o ->
    InvitationRequest
      <$> o .:? "locale"
      <*> o .:? "role"
      <*> o .:? "name"
      <*> o .: "email"
      <*> o .:? "phone"

--------------------------------------------------------------------------------
-- Invitation

data Invitation = Invitation
  { inTeam :: TeamId,
    inRole :: Role,
    inInvitation :: InvitationId,
    inCreatedAt :: UTCTimeMillis,
    -- | this is always 'Just' for new invitations, but for
    -- migration it is allowed to be 'Nothing'.
    inCreatedBy :: Maybe UserId,
    inInviteeEmail :: Email,
    inInviteeName :: Maybe Name,
    inInviteePhone :: Maybe Phone,
    inInviteeUrl :: Maybe (URIRef Absolute)
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Invitation)

-- | (This is *not* the swagger model for the 'TeamInvitation' type (which does not exist),
-- but for the use of 'Invitation' under @/teams/{tid}/invitations@.)
modelTeamInvitation :: Doc.Model
modelTeamInvitation = Doc.defineModel "TeamInvitation" $ do
  Doc.description "An invitation to join a team on Wire"
  Doc.property "team" Doc.bytes' $
    Doc.description "Team ID of the inviting team"
  Doc.property "role" typeRole $ do
    Doc.description "Role of the invited user"
    Doc.optional
  Doc.property "id" Doc.bytes' $
    Doc.description "UUID used to refer the invitation"
  Doc.property "created_at" Doc.dateTime' $
    Doc.description "Timestamp of invitation creation"
  Doc.property "created_by" Doc.bytes' $ do
    Doc.description "ID of the inviting user"
    Doc.optional
  Doc.property "email" Doc.string' $
    Doc.description "Email of the invitee"
  Doc.property "name" Doc.string' $ do
    Doc.description "Name of the invitee (1 - 128 characters)"
    Doc.optional
  Doc.property "phone" Doc.string' $ do
    Doc.description "Phone number of the invitee, in the E.164 format"
    Doc.optional
  Doc.property "url" Doc.string' $ do
    Doc.description "URL of the invitation link to be sent to the invitee"
    Doc.optional

instance ToJSON Invitation where
  toJSON i =
    object
      [ "team" .= inTeam i,
        "role" .= inRole i,
        "id" .= inInvitation i,
        "created_at" .= inCreatedAt i,
        "created_by" .= inCreatedBy i,
        "email" .= inInviteeEmail i,
        "name" .= inInviteeName i,
        "phone" .= inInviteePhone i,
        "url" .= inInviteeUrl i
      ]

instance FromJSON Invitation where
  parseJSON = withObject "invitation" $ \o ->
    Invitation
      <$> o .: "team"
      -- clients, when leaving "role" empty, can leave the default role choice to us
      <*> o .:? "role" .!= defaultRole
      <*> o .: "id"
      <*> o .: "created_at"
      <*> o .:? "created_by"
      <*> o .: "email"
      <*> o .:? "name"
      <*> o .:? "phone"
      <*> o .:? "url"

--------------------------------------------------------------------------------
-- InvitationList

data InvitationList = InvitationList
  { ilInvitations :: [Invitation],
    ilHasMore :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform InvitationList)

modelTeamInvitationList :: Doc.Model
modelTeamInvitationList = Doc.defineModel "TeamInvitationList" $ do
  Doc.description "A list of sent team invitations."
  Doc.property "invitations" (Doc.unique $ Doc.array (Doc.ref modelTeamInvitation)) Doc.end
  Doc.property "has_more" Doc.bool' $
    Doc.description "Indicator that the server has more invitations than returned."

instance ToJSON InvitationList where
  toJSON (InvitationList l m) =
    object
      [ "invitations" .= l,
        "has_more" .= m
      ]

instance FromJSON InvitationList where
  parseJSON = withObject "InvitationList" $ \o ->
    InvitationList
      <$> o .: "invitations"
      <*> o .: "has_more"

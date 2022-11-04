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
    InvitationLocation (..),

    -- * Swagger
    modelTeamInvitation,
    modelTeamInvitationList,
    modelTeamInvitationRequest,
  )
where

import Control.Lens ((?~))
import qualified Data.Aeson as A
import Data.ByteString.Conversion
import Data.Id
import Data.Json.Util
import Data.Schema
import qualified Data.Swagger as S
import qualified Data.Swagger.Build.Api as Doc
import qualified Data.Text.Encoding as TE
import Imports
import Servant (FromHttpApiData (..), ToHttpApiData (..))
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
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via (Schema InvitationRequest)

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

instance ToSchema InvitationRequest where
  schema =
    objectWithDocModifier "InvitationRequest" (description ?~ "A request to join a team on Wire.") $
      InvitationRequest
        <$> irLocale
          .= optFieldWithDocModifier "locale" (description ?~ "Locale to use for the invitation.") (maybeWithDefault A.Null schema)
        <*> irRole
          .= optFieldWithDocModifier "role" (description ?~ "Role of the invitee (invited user).") (maybeWithDefault A.Null schema)
        <*> irInviteeName
          .= optFieldWithDocModifier "name" (description ?~ "Name of the invitee (1 - 128 characters).") (maybeWithDefault A.Null schema)
        <*> irInviteeEmail
          .= fieldWithDocModifier "email" (description ?~ "Email of the invitee.") schema
        <*> irInviteePhone
          .= optFieldWithDocModifier "phone" (description ?~ "Phone number of the invitee, in the E.164 format.") (maybeWithDefault A.Null schema)

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
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via (Schema Invitation)

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

instance ToSchema Invitation where
  schema =
    objectWithDocModifier "Invitation" (description ?~ "An invitation to join a team on Wire") $
      Invitation
        <$> inTeam
          .= fieldWithDocModifier "team" (description ?~ "Team ID of the inviting team") schema
        <*> inRole
          -- clients, when leaving "role" empty, can leave the default role choice to us
          .= (fromMaybe defaultRole <$> optFieldWithDocModifier "role" (description ?~ "Role of the invited user") schema)
        <*> inInvitation
          .= fieldWithDocModifier "id" (description ?~ "UUID used to refer the invitation") schema
        <*> inCreatedAt
          .= fieldWithDocModifier "created_at" (description ?~ "Timestamp of invitation creation") schema
        <*> inCreatedBy
          .= optFieldWithDocModifier "created_by" (description ?~ "ID of the inviting user") (maybeWithDefault A.Null schema)
        <*> inInviteeEmail
          .= fieldWithDocModifier "email" (description ?~ "Email of the invitee") schema
        <*> inInviteeName
          .= optFieldWithDocModifier "name" (description ?~ "Name of the invitee (1 - 128 characters)") (maybeWithDefault A.Null schema)
        <*> inInviteePhone
          .= optFieldWithDocModifier "phone" (description ?~ "Phone number of the invitee, in the E.164 format") (maybeWithDefault A.Null schema)
        <*> (fmap (TE.decodeUtf8 . serializeURIRef') . inInviteeUrl)
          .= optFieldWithDocModifier "url" (description ?~ "URL of the invitation link to be sent to the invitee") (maybeWithDefault A.Null urlSchema)
    where
      urlSchema = parsedText "URIRef Absolute" (runParser (uriParser strictURIParserOptions) . TE.encodeUtf8)

newtype InvitationLocation = InvitationLocation
  { unInvitationLocation :: ByteString
  }
  deriving stock (Eq, Show, Generic)

instance S.ToParamSchema InvitationLocation where
  toParamSchema _ =
    mempty
      & S.type_ ?~ S.SwaggerString
      & S.format ?~ "url"

instance FromHttpApiData InvitationLocation where
  parseUrlPiece = parseHeader . TE.encodeUtf8
  parseHeader = pure . InvitationLocation

instance ToHttpApiData InvitationLocation where
  toUrlPiece = TE.decodeUtf8 . toHeader
  toHeader = unInvitationLocation

--------------------------------------------------------------------------------
-- InvitationList

data InvitationList = InvitationList
  { ilInvitations :: [Invitation],
    ilHasMore :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform InvitationList)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via (Schema InvitationList)

modelTeamInvitationList :: Doc.Model
modelTeamInvitationList = Doc.defineModel "TeamInvitationList" $ do
  Doc.description "A list of sent team invitations."
  Doc.property "invitations" (Doc.unique $ Doc.array (Doc.ref modelTeamInvitation)) Doc.end
  Doc.property "has_more" Doc.bool' $
    Doc.description "Indicator that the server has more invitations than returned."

instance ToSchema InvitationList where
  schema =
    objectWithDocModifier "InvitationList" (description ?~ "A list of sent team invitations.") $
      InvitationList
        <$> ilInvitations
          .= field "invitations" (array schema)
        <*> ilHasMore
          .= fieldWithDocModifier "has_more" (description ?~ "Indicator that the server has more invitations than returned.") schema

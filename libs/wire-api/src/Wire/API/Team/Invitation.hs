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
    InvitationRequestV5 (..),
    Invitation (..),
    InvitationList (..),
    InvitationLocation (..),
    HeadInvitationByEmailResult (..),
    HeadInvitationsResponses,
  )
where

import Control.Lens ((?~))
import Data.Aeson qualified as A
import Data.ByteString.Conversion
import Data.Id
import Data.Json.Util
import Data.OpenApi qualified as S
import Data.SOP
import Data.Schema
import Data.Text.Encoding qualified as TE
import Imports
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import URI.ByteString
import Wire.API.Error
import Wire.API.Error.Brig
import Wire.API.Locale (Locale)
import Wire.API.Routes.MultiVerb
import Wire.API.Team.Role (Role, defaultRole)
import Wire.API.User.Identity (Email, Phone)
import Wire.API.User.Profile (Name)
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

--------------------------------------------------------------------------------
-- InvitationRequest

data InvitationRequest = InvitationRequest
  { locale :: Maybe Locale,
    role :: Maybe Role,
    inviteeName :: Maybe Name,
    inviteeEmail :: Email
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform InvitationRequest)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via (Schema InvitationRequest)

instance ToSchema InvitationRequest where
  schema =
    objectWithDocModifier "InvitationRequest" (description ?~ "A request to join a team on Wire.") $
      InvitationRequest
        <$> locale
          .= optFieldWithDocModifier "locale" (description ?~ "Locale to use for the invitation.") (maybeWithDefault A.Null schema)
        <*> role
          .= optFieldWithDocModifier "role" (description ?~ "Role of the invitee (invited user).") (maybeWithDefault A.Null schema)
        <*> inviteeName
          .= optFieldWithDocModifier "name" (description ?~ "Name of the invitee (1 - 128 characters).") (maybeWithDefault A.Null schema)
        <*> inviteeEmail
          .= fieldWithDocModifier "email" (description ?~ "Email of the invitee.") schema

data InvitationRequestV5 = InvitationRequestV5
  { irLocale :: Maybe Locale,
    irRole :: Maybe Role,
    irInviteeName :: Maybe Name,
    irInviteeEmail :: Email,
    irInviteePhone :: Maybe Phone
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform InvitationRequestV5)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via (Schema InvitationRequestV5)

instance ToSchema InvitationRequestV5 where
  schema =
    objectWithDocModifier "InvitationRequestV5" (description ?~ "A request to join a team on Wire.") $
      InvitationRequestV5
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
    inInviteeUrl :: Maybe (URIRef Absolute)
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Invitation)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via (Schema Invitation)

inInviteePhone :: Invitation -> Maybe Phone
inInviteePhone = const Nothing

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
        <* inInviteePhone
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
      & S.type_ ?~ S.OpenApiString
      & S.format ?~ "url"

instance FromHttpApiData InvitationLocation where
  parseUrlPiece = parseHeader . TE.encodeUtf8
  parseHeader = pure . InvitationLocation

instance ToHttpApiData InvitationLocation where
  toUrlPiece = TE.decodeUtf8 . toHeader
  toHeader = unInvitationLocation

data HeadInvitationByEmailResult
  = InvitationByEmail
  | InvitationByEmailNotFound
  | InvitationByEmailMoreThanOne

type HeadInvitationsResponses =
  '[ ErrorResponse 'PendingInvitationNotFound,
     ErrorResponse 'ConflictingInvitations,
     RespondEmpty 200 "Pending invitation exists."
   ]

instance AsUnion HeadInvitationsResponses HeadInvitationByEmailResult where
  toUnion InvitationByEmailNotFound = Z (I (dynError @(MapError 'PendingInvitationNotFound)))
  toUnion InvitationByEmailMoreThanOne = S (Z (I (dynError @(MapError 'ConflictingInvitations))))
  toUnion InvitationByEmail = S (S (Z (I ())))

  fromUnion (Z (I _)) = InvitationByEmailNotFound
  fromUnion (S (Z (I _))) = InvitationByEmailMoreThanOne
  fromUnion (S (S (Z (I ())))) = InvitationByEmail
  fromUnion (S (S (S x))) = case x of {}

--------------------------------------------------------------------------------
-- InvitationList

data InvitationList = InvitationList
  { ilInvitations :: [Invitation],
    ilHasMore :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform InvitationList)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via (Schema InvitationList)

instance ToSchema InvitationList where
  schema =
    objectWithDocModifier "InvitationList" (description ?~ "A list of sent team invitations.") $
      InvitationList
        <$> ilInvitations
          .= field "invitations" (array schema)
        <*> ilHasMore
          .= fieldWithDocModifier "has_more" (description ?~ "Indicator that the server has more invitations than returned.") schema

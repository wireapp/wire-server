{-# LANGUAGE OverloadedStrings #-}

module Brig.Types.Team.Invitation where

import Brig.Types.Common
import Data.Aeson
import Data.Id
import Data.Json.Util
import Galley.Types.Teams
import Imports

data InvitationRequest
  = InvitationRequest
      { irEmail :: !Email,
        irName :: !Name,
        irLocale :: !(Maybe Locale),
        irRole :: !(Maybe Role),
        irInviteeName :: !(Maybe Name),
        irPhone :: !(Maybe Phone)
      }
  deriving (Eq, Show)

data Invitation
  = Invitation
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

data InvitationList
  = InvitationList
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
      <*> o .:? "invitee_name"
      <*> o .:? "phone"

instance ToJSON InvitationRequest where
  toJSON i =
    object $
      [ "email" .= irEmail i,
        "inviter_name" .= irName i,
        "locale" .= irLocale i,
        "role" .= irRole i,
        "invitee_name" .= irInviteeName i,
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
      <*> o .:? "invitee_name"
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
        "invitee_name" .= inInviteeName i,
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

{-# LANGUAGE OverloadedStrings #-}

module Brig.Types.Team.Invitation where

import Brig.Types.Common
import Data.Aeson
import Data.Id
import Data.Json.Util
import Data.Time.Clock (UTCTime)

data InvitationRequest = InvitationRequest
    { irEmail    :: !Email
    , irName     :: !Name
    , irLocale   :: !(Maybe Locale)
    } deriving (Eq, Show)

data Invitation = Invitation
    { inTeam       :: !TeamId
    , inInvitation :: !InvitationId
    , inIdentity   :: !Email
    , inCreatedAt  :: !UTCTime
    } deriving (Eq, Show)

data InvitationList = InvitationList
    { ilInvitations :: [Invitation]
    , ilHasMore     :: !Bool
    } deriving (Eq, Show)

instance FromJSON InvitationRequest where
    parseJSON = withObject "invitation-request" $ \o ->
        InvitationRequest <$> o .:  "email"
                          <*> o .:  "inviter_name"
                          <*> o .:? "locale"

instance ToJSON InvitationRequest where
    toJSON i = object [ "email"        .= irEmail i
                      , "inviter_name" .= irName i
                      , "locale"       .= irLocale i
                      ]

instance FromJSON Invitation where
    parseJSON = withObject "invitation" $ \o ->
        Invitation <$> o .: "team"
                   <*> o .: "id"
                   <*> o .: "email"
                   <*> o .: "created_at"

instance ToJSON Invitation where
    toJSON i = object [ "team"       .= inTeam i
                      , "id"         .= inInvitation i
                      , "email"      .= inIdentity i
                      , "created_at" .= UTCTimeMillis (inCreatedAt i)
                      ]

instance ToJSON InvitationList where
    toJSON (InvitationList l m) = object
        [ "invitations" .= l
        , "has_more"    .= m
        ]

instance FromJSON InvitationList where
    parseJSON = withObject "InvitationList" $ \o ->
        InvitationList <$> o .: "invitations"
                       <*> o .: "has_more"

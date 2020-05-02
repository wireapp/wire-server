{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

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

module Wire.API.Team.Event
  ( -- * Event
    Event,
    newEvent,
    eventType,
    eventTime,
    eventTeam,
    eventData,

    -- * EventType
    EventType (..),

    -- * EventData
    EventData (..),

    -- * TeamUpdateData
    TeamUpdateData,
    newTeamUpdateData,
    nameUpdate,
    iconUpdate,
    iconKeyUpdate,
  )
where

import Control.Lens (makeLenses)
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HashMap
import Data.Id (ConvId, TeamId, UserId)
import Data.Json.Util
import Data.Range
import Data.Time (UTCTime)
import Imports
import Wire.API.Team

--------------------------------------------------------------------------------
-- Event

data Event = Event
  { _eventType :: EventType,
    _eventTeam :: TeamId,
    _eventTime :: UTCTime,
    _eventData :: Maybe EventData
  }
  deriving (Eq, Generic)

newEvent :: EventType -> TeamId -> UTCTime -> Event
newEvent typ tid tme = Event typ tid tme Nothing

instance ToJSON Event where
  toJSON = Object . toJSONObject

instance ToJSONObject Event where
  toJSONObject e =
    HashMap.fromList
      [ "type" .= _eventType e,
        "team" .= _eventTeam e,
        "time" .= _eventTime e,
        "data" .= _eventData e
      ]

instance FromJSON Event where
  parseJSON = withObject "event" $ \o -> do
    ty <- o .: "type"
    dt <- o .:? "data"
    Event ty <$> o .: "team"
      <*> o .: "time"
      <*> parseEventData ty dt

--------------------------------------------------------------------------------
-- EventType

data EventType
  = TeamCreate
  | TeamDelete
  | TeamUpdate
  | MemberJoin
  | MemberLeave
  | MemberUpdate
  | ConvCreate
  | ConvDelete
  deriving (Eq, Show, Generic)

instance ToJSON EventType where
  toJSON TeamCreate = String "team.create"
  toJSON TeamDelete = String "team.delete"
  toJSON TeamUpdate = String "team.update"
  toJSON MemberJoin = String "team.member-join"
  toJSON MemberUpdate = String "team.member-update"
  toJSON MemberLeave = String "team.member-leave"
  toJSON ConvCreate = String "team.conversation-create"
  toJSON ConvDelete = String "team.conversation-delete"

instance FromJSON EventType where
  parseJSON (String "team.create") = pure TeamCreate
  parseJSON (String "team.delete") = pure TeamDelete
  parseJSON (String "team.update") = pure TeamUpdate
  parseJSON (String "team.member-join") = pure MemberJoin
  parseJSON (String "team.member-update") = pure MemberUpdate
  parseJSON (String "team.member-leave") = pure MemberLeave
  parseJSON (String "team.conversation-create") = pure ConvCreate
  parseJSON (String "team.conversation-delete") = pure ConvDelete
  parseJSON other = fail $ "Unknown event type: " <> show other

--------------------------------------------------------------------------------
-- EventData

data EventData
  = EdTeamCreate Team
  | EdTeamUpdate TeamUpdateData
  | EdMemberJoin UserId
  | EdMemberLeave UserId
  | EdMemberUpdate UserId (Maybe Permissions)
  | EdConvCreate ConvId
  | EdConvDelete ConvId
  deriving (Eq, Show, Generic)

instance ToJSON EventData where
  toJSON (EdTeamCreate tem) = toJSON tem
  toJSON (EdMemberJoin usr) = object ["user" .= usr]
  toJSON (EdMemberUpdate usr mPerm) =
    object $
      "user" .= usr
        # "permissions" .= mPerm
        # []
  toJSON (EdMemberLeave usr) = object ["user" .= usr]
  toJSON (EdConvCreate cnv) = object ["conv" .= cnv]
  toJSON (EdConvDelete cnv) = object ["conv" .= cnv]
  toJSON (EdTeamUpdate upd) = toJSON upd

parseEventData :: EventType -> Maybe Value -> Parser (Maybe EventData)
parseEventData MemberJoin Nothing = fail "missing event data for type 'team.member-join'"
parseEventData MemberJoin (Just j) = do
  let f o = Just . EdMemberJoin <$> o .: "user"
  withObject "member join data" f j
parseEventData MemberUpdate Nothing = fail "missing event data for type 'team.member-update"
parseEventData MemberUpdate (Just j) = do
  let f o = Just <$> (EdMemberUpdate <$> o .: "user" <*> o .:? "permissions")
  withObject "member update data" f j
parseEventData MemberLeave Nothing = fail "missing event data for type 'team.member-leave'"
parseEventData MemberLeave (Just j) = do
  let f o = Just . EdMemberLeave <$> o .: "user"
  withObject "member leave data" f j
parseEventData ConvCreate Nothing = fail "missing event data for type 'team.conversation-create"
parseEventData ConvCreate (Just j) = do
  let f o = Just . EdConvCreate <$> o .: "conv"
  withObject "conversation create data" f j
parseEventData ConvDelete Nothing = fail "missing event data for type 'team.conversation-delete"
parseEventData ConvDelete (Just j) = do
  let f o = Just . EdConvDelete <$> o .: "conv"
  withObject "conversation delete data" f j
parseEventData TeamCreate Nothing = fail "missing event data for type 'team.create'"
parseEventData TeamCreate (Just j) = Just . EdTeamCreate <$> parseJSON j
parseEventData TeamUpdate Nothing = fail "missing event data for type 'team.update'"
parseEventData TeamUpdate (Just j) = Just . EdTeamUpdate <$> parseJSON j
parseEventData _ Nothing = pure Nothing
parseEventData t (Just _) = fail $ "unexpected event data for type " <> show t

--------------------------------------------------------------------------------
-- TeamUpdateData

data TeamUpdateData = TeamUpdateData
  { _nameUpdate :: Maybe (Range 1 256 Text),
    _iconUpdate :: Maybe (Range 1 256 Text),
    _iconKeyUpdate :: Maybe (Range 1 256 Text)
  }
  deriving (Eq, Show, Generic)

newTeamUpdateData :: TeamUpdateData
newTeamUpdateData = TeamUpdateData Nothing Nothing Nothing

instance ToJSON TeamUpdateData where
  toJSON u =
    object $
      "name" .= _nameUpdate u
        # "icon" .= _iconUpdate u
        # "icon_key" .= _iconKeyUpdate u
        # []

instance FromJSON TeamUpdateData where
  parseJSON = withObject "team update data" $ \o -> do
    name <- o .:? "name"
    icon <- o .:? "icon"
    icon_key <- o .:? "icon_key"
    when (isNothing name && isNothing icon && isNothing icon_key) $
      fail "TeamUpdateData: no update data specified"
    either fail pure $
      TeamUpdateData <$> maybe (pure Nothing) (fmap Just . checkedEitherMsg "name") name
        <*> maybe (pure Nothing) (fmap Just . checkedEitherMsg "icon") icon
        <*> maybe (pure Nothing) (fmap Just . checkedEitherMsg "icon_key") icon_key

makeLenses ''Event

makeLenses ''TeamUpdateData

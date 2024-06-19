{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

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

module Wire.API.Event.Team
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
  )
where

import Control.Lens (makeLenses, (?~))
import Data.Aeson hiding (object, (.=))
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.Id (ConvId, TeamId, UserId)
import Data.Json.Util
import Data.OpenApi qualified as S
import Data.Schema
import Data.Time (UTCTime)
import Imports
import Test.QuickCheck qualified as QC
import Wire.API.Team (Team, TeamUpdateData)
import Wire.API.Team.Permission (Permissions)
import Wire.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))

--------------------------------------------------------------------------------
-- Event

data Event = Event
  { _eventTeam :: TeamId,
    _eventTime :: UTCTime,
    _eventData :: EventData
  }
  deriving stock (Eq, Show, Generic)

instance ToSchema Event where
  schema =
    object "Event" $
      Event
        <$> _eventTeam .= field "team" schema
        <*> _eventTime .= field "time" utcTimeSchema
        <*> _eventData .= fieldWithDocModifier "data" (description ?~ dataFieldDesc) schema
        <* eventType .= field "version" schema
    where
      dataFieldDesc = "FUTUREWORK: this part of the docs is lying; we're working on it!"

instance S.ToSchema Event where
  declareNamedSchema = schemaToSwagger

eventType :: Event -> EventType
eventType = eventDataType . _eventData

newEvent :: TeamId -> UTCTime -> EventData -> Event
newEvent = Event

instance ToJSON Event where
  toJSON = A.Object . toJSONObject

instance ToJSONObject Event where
  toJSONObject e =
    KeyMap.fromList
      [ "type" A..= eventType e,
        "team" A..= _eventTeam e,
        "time" A..= _eventTime e,
        "data" A..= _eventData e
      ]

instance FromJSON Event where
  parseJSON = withObject "event" $ \o -> do
    ty <- o .: "type"
    dt <- o .:? "data"
    Event
      <$> o .: "team"
      <*> o .: "time"
      <*> parseEventData ty dt

instance Arbitrary Event where
  arbitrary = do
    typ <- arbitrary
    Event
      <$> arbitrary
      <*> arbitrary
      <*> genEventData typ

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
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform EventType)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema EventType

instance ToSchema EventType where
  schema =
    enum @Text "EventType" $
      mconcat
        [ element "team.create" TeamCreate,
          element "team.delete" TeamDelete,
          element "team.update" TeamUpdate,
          element "team.member-join" MemberJoin,
          element "team.member-leave" MemberLeave,
          element "team.member-update" MemberUpdate,
          element "team.conversation-create" ConvCreate,
          element "team.conversation-delete" ConvDelete
        ]

--------------------------------------------------------------------------------
-- EventData

data EventData
  = EdTeamCreate Team
  | EdTeamDelete
  | EdTeamUpdate TeamUpdateData
  | EdMemberJoin UserId
  | EdMemberLeave UserId
  | EdMemberUpdate UserId (Maybe Permissions)
  | EdConvCreate ConvId
  | EdConvDelete ConvId
  deriving stock (Eq, Show, Generic)

-- FUTUREWORK: this is outright wrong; see "Wire.API.Event.Conversation" on how to do this properly.
instance ToSchema EventData where
  schema =
    object "EventData" $
      EdTeamCreate
        <$> (undefined :: EventData -> Team) .= field "team" schema

instance ToJSON EventData where
  toJSON (EdTeamCreate tem) = toJSON tem
  toJSON EdTeamDelete = Null
  toJSON (EdMemberJoin usr) = A.object ["user" A..= usr]
  toJSON (EdMemberUpdate usr mPerm) =
    A.object $
      "user"
        A..= usr
        # "permissions"
        A..= mPerm
        # []
  toJSON (EdMemberLeave usr) = A.object ["user" A..= usr]
  toJSON (EdConvCreate cnv) = A.object ["conv" A..= cnv]
  toJSON (EdConvDelete cnv) = A.object ["conv" A..= cnv]
  toJSON (EdTeamUpdate upd) = toJSON upd

eventDataType :: EventData -> EventType
eventDataType (EdTeamCreate _) = TeamCreate
eventDataType EdTeamDelete = TeamDelete
eventDataType (EdTeamUpdate _) = TeamUpdate
eventDataType (EdMemberJoin _) = MemberJoin
eventDataType (EdMemberLeave _) = MemberLeave
eventDataType (EdMemberUpdate _ _) = MemberUpdate
eventDataType (EdConvCreate _) = ConvCreate
eventDataType (EdConvDelete _) = ConvDelete

parseEventData :: EventType -> Maybe Value -> Parser EventData
parseEventData MemberJoin Nothing = fail "missing event data for type 'team.member-join'"
parseEventData MemberJoin (Just j) = do
  let f o = EdMemberJoin <$> o .: "user"
  withObject "member join data" f j
parseEventData MemberUpdate Nothing = fail "missing event data for type 'team.member-update"
parseEventData MemberUpdate (Just j) = do
  let f o = EdMemberUpdate <$> o .: "user" <*> o .:? "permissions"
  withObject "member update data" f j
parseEventData MemberLeave Nothing = fail "missing event data for type 'team.member-leave'"
parseEventData MemberLeave (Just j) = do
  let f o = EdMemberLeave <$> o .: "user"
  withObject "member leave data" f j
parseEventData ConvCreate Nothing = fail "missing event data for type 'team.conversation-create"
parseEventData ConvCreate (Just j) = do
  let f o = EdConvCreate <$> o .: "conv"
  withObject "conversation create data" f j
parseEventData ConvDelete Nothing = fail "missing event data for type 'team.conversation-delete"
parseEventData ConvDelete (Just j) = do
  let f o = EdConvDelete <$> o .: "conv"
  withObject "conversation delete data" f j
parseEventData TeamCreate Nothing = fail "missing event data for type 'team.create'"
parseEventData TeamCreate (Just j) = EdTeamCreate <$> parseJSON j
parseEventData TeamUpdate Nothing = fail "missing event data for type 'team.update'"
parseEventData TeamUpdate (Just j) = EdTeamUpdate <$> parseJSON j
parseEventData _ Nothing = pure EdTeamDelete
parseEventData t (Just _) = fail $ "unexpected event data for type " <> show t

genEventData :: EventType -> QC.Gen EventData
genEventData = \case
  TeamCreate -> EdTeamCreate <$> arbitrary
  TeamDelete -> pure EdTeamDelete
  TeamUpdate -> EdTeamUpdate <$> arbitrary
  MemberJoin -> EdMemberJoin <$> arbitrary
  MemberLeave -> EdMemberLeave <$> arbitrary
  MemberUpdate -> EdMemberUpdate <$> arbitrary <*> arbitrary
  ConvCreate -> EdConvCreate <$> arbitrary
  ConvDelete -> EdConvDelete <$> arbitrary

makeLenses ''Event

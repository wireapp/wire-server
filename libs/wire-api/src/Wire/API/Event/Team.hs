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

    -- * Swagger
    modelEvent,
    modelMemberEvent,
    modelMemberData,
    modelConvEvent,
    modelConversationData,
    modelUpdateEvent,
    typeEventType,
  )
where

import Control.Lens (makeLenses, (?~))
import Data.Aeson hiding (object, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser)
import Data.Id (ConvId, TeamId, UserId)
import Data.Json.Util
import Data.Schema
import qualified Data.Swagger as S
import qualified Data.Swagger.Build.Api as Doc
import Data.Time (UTCTime)
import Imports
import qualified Test.QuickCheck as QC
import Wire.API.Team (Team, TeamUpdateData, modelUpdateData)
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
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema Event

instance ToSchema Event where
  schema =
    object "Team.Event" $ eventObjectSchema

eventObjectSchema :: ObjectSchema SwaggerDoc Event
eventObjectSchema =
  Event
    <$> _eventTeam .= field "team" schema
    <*> _eventTime .= field "time" utcTimeSchema
    <*> _eventData .= fieldWithDocModifier "data" (description ?~ dataFieldDesc) schema
    <* eventType .= field "version" schema
  where
    dataFieldDesc = "FUTUREWORK: this part of the docs is lying; we're working on it!"

-- instance ToJSON Event where
--   toJSON = A.Object . toJSONObject

-- instance FromJSON Event where
--   parseJSON = withObject "event" $ \o -> do
--     ty <- o .: "type"
--     dt <- o .:? "data"
--     Event
--       <$> o .: "team"
--       <*> o .: "time"
--       <*> _parseEventData ty dt
eventType :: Event -> EventType
eventType = eventDataType . _eventData

newEvent :: TeamId -> UTCTime -> EventData -> Event
newEvent = Event

instance ToJSONObject Event where
  toJSONObject =
    KeyMap.fromList
      . fromMaybe []
      . schemaOut eventObjectSchema

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
    enum @Text "Team.EventType" $
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
      "user" A..= usr
        # "permissions" A..= mPerm
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

_parseEventData :: EventType -> Maybe Value -> Parser EventData
_parseEventData MemberJoin Nothing = fail "missing event data for type 'team.member-join'"
_parseEventData MemberJoin (Just j) = do
  let f o = EdMemberJoin <$> o .: "user"
  withObject "member join data" f j
_parseEventData MemberUpdate Nothing = fail "missing event data for type 'team.member-update"
_parseEventData MemberUpdate (Just j) = do
  let f o = EdMemberUpdate <$> o .: "user" <*> o .:? "permissions"
  withObject "member update data" f j
_parseEventData MemberLeave Nothing = fail "missing event data for type 'team.member-leave'"
_parseEventData MemberLeave (Just j) = do
  let f o = EdMemberLeave <$> o .: "user"
  withObject "member leave data" f j
_parseEventData ConvCreate Nothing = fail "missing event data for type 'team.conversation-create"
_parseEventData ConvCreate (Just j) = do
  let f o = EdConvCreate <$> o .: "conv"
  withObject "conversation create data" f j
_parseEventData ConvDelete Nothing = fail "missing event data for type 'team.conversation-delete"
_parseEventData ConvDelete (Just j) = do
  let f o = EdConvDelete <$> o .: "conv"
  withObject "conversation delete data" f j
_parseEventData TeamCreate Nothing = fail "missing event data for type 'team.create'"
_parseEventData TeamCreate (Just j) = EdTeamCreate <$> parseJSON j
_parseEventData TeamUpdate Nothing = fail "missing event data for type 'team.update'"
_parseEventData TeamUpdate (Just j) = EdTeamUpdate <$> parseJSON j
_parseEventData _ Nothing = pure EdTeamDelete
_parseEventData t (Just _) = fail $ "unexpected event data for type " <> show t

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

----------------------------------------------------------------------
-- swagger1.2 stuff, to be removed in
-- https://wearezeta.atlassian.net/browse/SQSERVICES-1096

typeEventType :: Doc.DataType
typeEventType =
  Doc.string $
    Doc.enum
      [ "team.create",
        "team.delete",
        "team.update",
        "team.member-join",
        "team.member-leave",
        "team.conversation-create",
        "team.conversation-delete"
      ]

modelEvent :: Doc.Model
modelEvent = Doc.defineModel "TeamEvent" $ do
  Doc.description "team event data"
  Doc.property "type" typeEventType $
    Doc.description "event type"
  Doc.property "team" Doc.bytes' $
    Doc.description "team ID"
  Doc.property "time" Doc.dateTime' $
    Doc.description "date and time this event occurred"
  -- This doesn't really seem to work in swagger-ui.
  -- The children/subTypes are not displayed.
  Doc.children
    "type"
    [ modelMemberEvent,
      modelConvEvent,
      modelUpdateEvent
    ]

modelMemberEvent :: Doc.Model
modelMemberEvent = Doc.defineModel "TeamMemberEvent" $ do
  Doc.description "team member event"
  Doc.property "data" (Doc.ref modelMemberData) $ Doc.description "member data"

modelMemberData :: Doc.Model
modelMemberData =
  Doc.defineModel "MemberData" $
    Doc.property "user" Doc.bytes' $
      Doc.description "user ID"

modelConvEvent :: Doc.Model
modelConvEvent = Doc.defineModel "TeamConversationEvent" $ do
  Doc.description "team conversation event"
  Doc.property "data" (Doc.ref modelConversationData) $ Doc.description "conversation data"

modelConversationData :: Doc.Model
modelConversationData =
  Doc.defineModel "ConversationData" $
    Doc.property "conv" Doc.bytes' $
      Doc.description "conversation ID"

modelUpdateEvent :: Doc.Model
modelUpdateEvent = Doc.defineModel "TeamUpdateEvent" $ do
  Doc.description "team update event"
  Doc.property "data" (Doc.ref modelUpdateData) $ Doc.description "update data"

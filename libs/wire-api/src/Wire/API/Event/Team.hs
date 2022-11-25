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

import Control.Lens (makeLenses, makePrisms, _1)
import Data.Aeson hiding (object, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KeyMap
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

eventType :: Event -> EventType
eventType = eventDataType . _eventData

newEvent :: TeamId -> UTCTime -> EventData -> Event
newEvent = Event

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
  deriving stock (Eq, Show, Generic, Enum, Bounded)
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

eventDataType :: EventData -> EventType
eventDataType (EdTeamCreate _) = TeamCreate
eventDataType EdTeamDelete = TeamDelete
eventDataType (EdTeamUpdate _) = TeamUpdate
eventDataType (EdMemberJoin _) = MemberJoin
eventDataType (EdMemberLeave _) = MemberLeave
eventDataType (EdMemberUpdate _ _) = MemberUpdate
eventDataType (EdConvCreate _) = ConvCreate
eventDataType (EdConvDelete _) = ConvDelete

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
makePrisms ''EventData

instance ToSchema EventData where
  schema = object "Team.EventData" $ eventDataObjectSchema

deriving via (Schema EventData) instance (A.ToJSON EventData)

deriving via (Schema EventData) instance (A.FromJSON EventData)

deriving via (Schema EventData) instance (S.ToSchema EventData)

eventDataObjectSchema :: ObjectSchema SwaggerDoc EventData
eventDataObjectSchema = snd <$> toTagged .= taggedEventDataSchema
  where
    toTagged :: EventData -> (EventType, EventData)
    toTagged ed = (eventDataType ed, ed)

    taggedEventDataSchema :: ObjectSchema SwaggerDoc (EventType, EventData)
    taggedEventDataSchema =
      bind
        (fst .= field "type" schema)
        (snd .= fieldOver _1 "data" edata)
      where
        edata = dispatch $ \case
          TeamCreate -> tag _EdTeamCreate (unnamed schema)
          TeamDelete -> tag _EdTeamDelete null_
          TeamUpdate -> tag _EdTeamUpdate (unnamed schema)
          MemberJoin -> tag _EdMemberJoin (unnamed $ object "UserId" (field "user" schema))
          MemberLeave -> tag _EdMemberLeave (unnamed $ object "UserId" (field "user" schema))
          MemberUpdate -> tag _EdMemberUpdate (unnamed $ object "(UserId, Maybe Permissions)" memberUpdateObjectSchema)
          ConvCreate -> tag _EdConvCreate (unnamed $ object "ConvId" (field "conv" schema))
          ConvDelete -> tag _EdConvDelete (unnamed $ object "ConvId" (field "conv" schema))

        memberUpdateObjectSchema :: ObjectSchemaP SwaggerDoc (UserId, Maybe Permissions) (UserId, Maybe Permissions)
        memberUpdateObjectSchema =
          (,)
            <$> fst .= field "user" schema
            <*> snd .= maybe_ (optField "permissions" schema)

eventObjectSchema :: ObjectSchema SwaggerDoc Event
eventObjectSchema =
  Event
    <$> _eventTeam .= field "team" schema
    <*> _eventTime .= field "time" utcTimeSchema
    <*> _eventData .= eventDataObjectSchema

instance ToSchema Event where
  schema =
    object "Team.Event" $ eventObjectSchema

instance ToJSONObject Event where
  toJSONObject =
    KeyMap.fromList
      . fromMaybe []
      . schemaOut eventObjectSchema

deriving via (Schema Event) instance (A.ToJSON Event)

deriving via (Schema Event) instance (A.FromJSON Event)

deriving via (Schema Event) instance (S.ToSchema Event)

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

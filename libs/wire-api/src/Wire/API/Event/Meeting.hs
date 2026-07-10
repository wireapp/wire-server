{-# LANGUAGE StrictData #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Event.Meeting
  ( -- * Event
    Event (..),
    newEvent,
    eventType,

    -- * EventType
    EventType (..),
  )
where

import Data.Aeson (FromJSON, ToJSON (toJSON), Value (Object))
import Data.Id (MeetingId)
import Data.Json.Util (ToJSONObject (toJSONObject), utcTimeSchema)
import Data.OpenApi qualified as S
import Data.Qualified (Qualified)
import Data.Schema
import Data.Time.Clock (UTCTime)
import Imports
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

--------------------------------------------------------------------------------
-- EventType

data EventType = MeetingCreate | MeetingUpdate | MeetingDelete
  deriving stock (Eq, Show, Generic, Enum, Bounded, Ord)
  deriving (Arbitrary) via (GenericUniform EventType)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema EventType

instance ToSchema EventType where
  schema =
    enum @Text $
      mconcat
        [ element "meeting.create" MeetingCreate,
          element "meeting.update" MeetingUpdate,
          element "meeting.delete" MeetingDelete
        ]

--------------------------------------------------------------------------------
-- Event

data Event = Event
  { evtType :: EventType,
    evtTime :: UTCTime,
    evtQualifiedId :: Qualified MeetingId
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema Event
  deriving (Arbitrary) via (GenericUniform Event)

instance ToSchema Event where
  schema =
    object $
      Event
        <$> (.evtType) .= field "type" schema
        <*> (.evtTime) .= field "time" utcTimeSchema
        <*> (.evtQualifiedId) .= field "qualified_id" schema

instance ToJSONObject Event where
  toJSONObject e = case toJSON e of
    Object o -> o
    _ -> KeyMap.fromList []

newEvent :: UTCTime -> EventType -> Qualified MeetingId -> Event
newEvent time ty qid = Event ty time qid

eventType :: Event -> EventType
eventType = (.evtType)

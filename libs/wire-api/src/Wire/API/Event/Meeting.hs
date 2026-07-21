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

{-# LANGUAGE StrictData #-}

module Wire.API.Event.Meeting
  ( -- * Event
    Event (..),
    EventType (..),

    -- * Envelope
    EventFrom (..)
  )
where

import Control.Applicative (optional)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Id
import Data.Json.Util
import Data.OpenApi qualified as S
import Data.Qualified
import Data.Schema
import Data.Time (UTCTime)
import Imports
import Wire.API.Event.Conversation (EventFrom (..), eventFromUserId, eventVia, mkEventFrom)
import Wire.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))

--------------------------------------------------------------------------------
-- EventType

data EventType = Create | Update | Delete
  deriving stock (Eq, Show, Generic, Enum, Bounded, Ord)
  deriving (Arbitrary) via (GenericUniform EventType)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema EventType

instance ToSchema EventType where
  schema =
    enum @Text $
      mconcat
        [ element "meeting.create" Create,
          element "meeting.update" Update,
          element "meeting.delete" Delete
        ]

--------------------------------------------------------------------------------
-- Event

-- | A self-contained meeting lifecycle event. Unlike conversation events, the
-- meeting's qualified id is carried flat at the envelope top level
-- (@qualified_id@), with no @data@ wrapper. The 'EventFrom' envelope helper is
-- shared with "Wire.API.Event.Conversation".
data Event = Event
  { evtType :: EventType,
    evtMeeting :: Qualified MeetingId,
    evtConv :: Qualified ConvId,
    evtFrom :: EventFrom,
    evtTime :: UTCTime,
    evtTeam :: Maybe TeamId
  }
  deriving stock (Eq, Show, Generic)

instance Arbitrary Event where
  arbitrary =
    Event
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> (milli <$> arbitrary)
      <*> arbitrary
    where
      milli = fromUTCTimeMillis . toUTCTimeMillis

instance ToSchema Event where
  schema = object eventObjectSchema

eventObjectSchema :: ObjectSchema SwaggerDoc Event
eventObjectSchema =
  mk
    <$> evtType .= field "type" schema
    <*> evtMeeting .= field "qualified_id" schema
    <* (qUnqualified . evtConv) .= optional (field "conversation" schema)
    <*> evtConv .= field "qualified_conversation" schema
    <* (qUnqualified . eventFromUserId . evtFrom) .= optional (field "from" schema)
    <*> (eventFromUserId . evtFrom) .= field "qualified_from" schema
    <*> (eventVia . evtFrom) .= field "via" schema
    <*> (toUTCTimeMillis . evtTime) .= field "time" (fromUTCTimeMillis <$> schema)
    <*> evtTeam .= maybe_ (optField "team" schema)
  where
    mk typ meeting cid uid evVia tm tid =
      Event typ meeting cid (mkEventFrom evVia uid) tm tid

instance ToJSONObject Event where
  toJSONObject =
    KeyMap.fromList
      . fromMaybe []
      . schemaOut eventObjectSchema

instance FromJSON Event where
  parseJSON = schemaParseJSON

instance ToJSON Event where
  toJSON = schemaToJSON

instance S.ToSchema Event where
  declareNamedSchema = schemaToSwagger


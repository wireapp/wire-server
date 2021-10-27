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

module Wire.API.Event.FeatureConfig
  ( Event (..),
    EventType (..),
    EventData (..),
  )
where

import Control.Arrow ((&&&))
import Control.Lens (makePrisms, _1)
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.HashMap.Strict as HashMap
import Data.Json.Util (ToJSONObject (..))
import Data.Schema
import qualified Data.Swagger as S
import Imports
import Wire.API.Team.Feature (TeamFeatureAppLockConfig, TeamFeatureClassifiedDomainsConfig, TeamFeatureName (..), TeamFeatureSelfDeletingMessagesConfig, TeamFeatureStatusNoConfig, TeamFeatureStatusWithConfig)

data Event = Event
  { _eventType :: EventType,
    _eventFeatureName :: TeamFeatureName,
    _eventData :: EventData
  }
  deriving (Eq, Show, Generic)

data EventType = Update
  deriving (Eq, Show)

instance ToSchema EventType where
  schema =
    enum @Text "EventType" $
      mconcat
        [ element "feature-config.update" Update
        ]

data EventData
  = EdFeatureWithoutConfigChanged TeamFeatureStatusNoConfig
  | EdFeatureApplockChanged (TeamFeatureStatusWithConfig TeamFeatureAppLockConfig)
  | EdFeatureClassifiedDomainsChanged (TeamFeatureStatusWithConfig TeamFeatureClassifiedDomainsConfig)
  | EdFeatureSelfDeletingMessagesChanged (TeamFeatureStatusWithConfig TeamFeatureSelfDeletingMessagesConfig)
  deriving (Eq, Show, Generic)

makePrisms ''EventData

taggedEventDataSchema :: ObjectSchema SwaggerDoc (TeamFeatureName, EventData)
taggedEventDataSchema =
  bind
    (fst .= field "name" schema)
    (snd .= fieldOver _1 "data" edata)
  where
    edata = dispatch $ \case
      TeamFeatureLegalHold -> tag _EdFeatureWithoutConfigChanged (unnamed schema)
      TeamFeatureSSO -> tag _EdFeatureWithoutConfigChanged (unnamed schema)
      TeamFeatureSearchVisibility -> tag _EdFeatureWithoutConfigChanged (unnamed schema)
      TeamFeatureValidateSAMLEmails -> tag _EdFeatureWithoutConfigChanged (unnamed schema)
      TeamFeatureDigitalSignatures -> tag _EdFeatureWithoutConfigChanged (unnamed schema)
      TeamFeatureAppLock -> tag _EdFeatureApplockChanged (unnamed schema)
      TeamFeatureFileSharing -> tag _EdFeatureWithoutConfigChanged (unnamed schema)
      TeamFeatureClassifiedDomains -> tag _EdFeatureClassifiedDomainsChanged (unnamed schema)
      TeamFeatureConferenceCalling -> tag _EdFeatureWithoutConfigChanged (unnamed schema)
      TeamFeatureSelfDeletingMessages -> tag _EdFeatureSelfDeletingMessagesChanged (unnamed schema)

eventObjectSchema :: ObjectSchema SwaggerDoc Event
eventObjectSchema =
  mkEvent
    <$> (_eventFeatureName &&& _eventData) .= taggedEventDataSchema
    <*> _eventType .= field "type" schema
  where
    mkEvent :: (TeamFeatureName, EventData) -> EventType -> Event
    mkEvent (feature, eventData) eventType = Event eventType feature eventData

instance ToSchema Event where
  schema = object "Event" eventObjectSchema

instance ToJSONObject Event where
  toJSONObject =
    HashMap.fromList
      . fromMaybe []
      . schemaOut eventObjectSchema

instance FromJSON Event where
  parseJSON = schemaParseJSON

instance ToJSON Event where
  toJSON = schemaToJSON

instance S.ToSchema Event where
  declareNamedSchema = schemaToSwagger

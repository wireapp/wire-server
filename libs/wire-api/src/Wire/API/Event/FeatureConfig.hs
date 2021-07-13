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

module Wire.API.Event.FeatureConfig where

import Data.Schema (ObjectSchema, SwaggerDoc, ToSchema (..), element, enum)
import Imports
import Wire.API.Team.Feature (TeamFeatureName, TeamFeatureStatusValue)

data Event = Event
  { _eventType :: EventType,
    _eventFeatureName :: TeamFeatureName,
    _eventData :: EventData
  }

-- TODO: schema
-- {
--   "type": <schema of EventType>
--   "data": <schema of EventData>
-- }
eventObjectSchema :: ObjectSchema SwaggerDoc Event
eventObjectSchema = undefined

data FeatureNoConfigChanged = FeatureNoConfigChanged TeamFeatureName TeamFeatureStatusValue

-- TODO: schema
-- {
--     "name": "fileSharing",
--     "status": "enabled" | "disabled"
-- }
featureNoConfigChangedObjectSchema :: ObjectSchema SwaggerDoc FeatureNoConfigChanged
featureNoConfigChangedObjectSchema = undefined

data EventData
  = EdFeatureNoConfigChanged FeatureNoConfigChanged
  | EdFeatureWithSome FeatureNoConfigChanged

-- dispatch on evenType and Feature Name
eventDataObjectSchema :: ObjectSchema SwaggerDoc EventData
eventDataObjectSchema = undefined

data EventType = Update
  deriving (Eq, Show)

instance ToSchema EventType where
  schema =
    enum @Text "EventType" $
      mconcat
        [ element "feature-config.update" Update
        ]

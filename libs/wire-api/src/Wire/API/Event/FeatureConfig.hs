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

module Wire.API.Event.FeatureConfig
  ( Event (..),
    EventType (..),
    mkUpdateEvent,
  )
where

import Data.Aeson (toJSON)
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Json.Util (ToJSONObject (toJSONObject))
import Data.OpenApi qualified as S
import Data.Schema
import GHC.TypeLits (KnownSymbol)
import Imports
import Test.QuickCheck.Gen
import Wire.API.Team.Feature
import Wire.Arbitrary (Arbitrary (..), GenericUniform (..))

data Event = Event
  { _eventType :: EventType,
    _eventFeatureName :: Text,
    _eventData :: A.Value
  }
  deriving (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON) via Schema Event

arbitraryFeature :: forall cfg. (IsFeatureConfig cfg, ToSchema cfg, Arbitrary cfg) => Gen A.Value
arbitraryFeature = toJSON <$> arbitrary @(LockableFeature cfg)

class AllArbitraryFeatures cfgs where
  allArbitraryFeatures :: [Gen A.Value]

instance AllArbitraryFeatures '[] where
  allArbitraryFeatures = []

instance
  ( IsFeatureConfig cfg,
    ToSchema cfg,
    Arbitrary cfg,
    AllArbitraryFeatures cfgs
  ) =>
  AllArbitraryFeatures (cfg : cfgs)
  where
  allArbitraryFeatures = arbitraryFeature @cfg : allArbitraryFeatures @cfgs

instance Arbitrary Event where
  arbitrary =
    Event
      <$> arbitrary
      <*> arbitrary
      <*> oneof (allArbitraryFeatures @Features)

data EventType = Update
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform EventType

instance ToSchema EventType where
  schema =
    enum @Text "EventType" $
      mconcat
        [ element "feature-config.update" Update
        ]

eventObjectSchema :: ObjectSchema SwaggerDoc Event
eventObjectSchema =
  Event
    <$> _eventType .= field "type" schema
    <*> _eventFeatureName .= field "name" schema
    <*> _eventData .= field "data" jsonValue

instance ToSchema Event where
  schema =
    object "Event" eventObjectSchema

instance ToJSONObject Event where
  toJSONObject =
    KeyMap.fromList
      . fromMaybe []
      . schemaOut eventObjectSchema

instance S.ToSchema Event where
  declareNamedSchema = schemaToSwagger

mkUpdateEvent :: forall cfg. (IsFeatureConfig cfg, ToSchema cfg, KnownSymbol (FeatureSymbol cfg)) => LockableFeature cfg -> Event
mkUpdateEvent ws = Event Update (featureName @cfg) (toJSON ws)

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
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Json.Util (ToJSONObject (toJSONObject))
import Data.Schema
import qualified Data.Swagger as S
import GHC.TypeLits (KnownSymbol)
import Imports
import Test.QuickCheck.Gen (oneof)
import Wire.API.Team.Feature
import Wire.Arbitrary (Arbitrary (..), GenericUniform (..))

data Event = Event
  { _eventType :: EventType,
    _eventFeatureName :: Text,
    _eventData :: A.Value
  }
  deriving (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON) via Schema Event

instance Arbitrary Event where
  arbitrary =
    do
      let arbConfig =
            oneof
              [ arbitrary @(WithStatus SSOConfig) <&> toJSON,
                arbitrary @(WithStatus SearchVisibilityAvailableConfig) <&> toJSON,
                arbitrary @(WithStatus ValidateSAMLEmailsConfig) <&> toJSON,
                arbitrary @(WithStatus DigitalSignaturesConfig) <&> toJSON,
                arbitrary @(WithStatus AppLockConfig) <&> toJSON,
                arbitrary @(WithStatus FileSharingConfig) <&> toJSON,
                arbitrary @(WithStatus ClassifiedDomainsConfig) <&> toJSON,
                arbitrary @(WithStatus ConferenceCallingConfig) <&> toJSON,
                arbitrary @(WithStatus SelfDeletingMessagesConfig) <&> toJSON,
                arbitrary @(WithStatus GuestLinksConfig) <&> toJSON,
                arbitrary @(WithStatus SndFactorPasswordChallengeConfig) <&> toJSON,
                arbitrary @(WithStatus SearchVisibilityInboundConfig) <&> toJSON,
                arbitrary @(WithStatus MLSConfig) <&> toJSON,
                arbitrary @(WithStatus ExposeInvitationURLsToTeamAdminConfig) <&> toJSON
              ]
      Event
        <$> arbitrary
        <*> arbitrary
        <*> arbConfig

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

mkUpdateEvent :: forall cfg. (IsFeatureConfig cfg, ToSchema cfg, KnownSymbol (FeatureSymbol cfg)) => WithStatus cfg -> Event
mkUpdateEvent ws = Event Update (featureName @cfg) (toJSON ws)

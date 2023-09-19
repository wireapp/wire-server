module Wire.API.Event.Federation
  ( Event (..),
    EventType (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Domain
import Data.Json.Util (ToJSONObject (toJSONObject))
import Data.OpenApi qualified as S
import Data.Schema
import Imports
import Wire.Arbitrary

data Event = Event
  { _eventType :: EventType,
    _eventDomain :: Domain
  }
  deriving (Eq, Show, Ord, Generic)

instance Arbitrary Event where
  arbitrary =
    Event
      <$> arbitrary
      <*> arbitrary

data EventType
  = FederationDelete
  deriving (Eq, Show, Ord, Generic)
  deriving (Arbitrary) via (GenericUniform EventType)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema EventType

instance ToSchema EventType where
  schema =
    enum @Text "EventType" $
      mconcat
        [ element "federation.delete" FederationDelete
        ]

eventObjectSchema :: ObjectSchema SwaggerDoc Event
eventObjectSchema =
  Event
    <$> _eventType .= field "type" schema
    <*> _eventDomain .= field "domain" schema

instance ToSchema Event where
  schema = object "Event" eventObjectSchema

instance ToJSONObject Event where
  toJSONObject =
    KeyMap.fromList
      . fromMaybe []
      . schemaOut eventObjectSchema

instance S.ToSchema Event where
  declareNamedSchema = schemaToSwagger

instance FromJSON Event where
  parseJSON = schemaParseJSON

instance ToJSON Event where
  toJSON = schemaToJSON

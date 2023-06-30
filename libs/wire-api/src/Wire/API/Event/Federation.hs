module Wire.API.Event.Federation
  ( Event (..)
  , EventType (..)
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Json.Util (ToJSONObject (toJSONObject))
import Data.Schema
import qualified Data.Swagger as S
import Imports
import Wire.Arbitrary
import Data.Domain

data Event = Event
  { _eventType :: EventType
  , _eventDomains :: [Domain]
  }
  deriving (Eq, Ord)

instance Arbitrary Event where
  arbitrary = Event
    <$> arbitrary
    <*> arbitrary

data EventType
  = FederationDelete
  deriving (Eq, Ord, Generic)
  deriving (Arbitrary) via (GenericUniform EventType)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema EventType

instance ToSchema EventType where
  schema =
    enum @Text "EventType" $
      mconcat
        [ element "federation.delete" FederationDelete
        ]

eventObjectSchema :: ObjectSchema SwaggerDoc Event
eventObjectSchema = Event
  <$> _eventType .= field "type" schema
  <*> _eventDomains .= field "domains" (array schema)

instance ToSchema Event where
  schema = object "Event" eventObjectSchema

instance ToJSONObject Event where
  toJSONObject =
    KeyMap.fromList
      . fromMaybe []
      . schemaOut eventObjectSchema

instance S.ToSchema Event where
  declareNamedSchema = schemaToSwagger
module Wire.API.Event.Gundeck where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Json.Util
import Imports
import Wire.API.Push.V2.Token

newtype PushRemove = PushRemove PushToken
  deriving (Eq, Show)

instance FromJSON PushRemove where
  parseJSON = withObject "push-removed object" $ \o ->
    PushRemove <$> o .: "token"

instance ToJSON PushRemove where
  toJSON = Object . toJSONObject

instance ToJSONObject PushRemove where
  toJSONObject (PushRemove t) =
    KeyMap.fromList
      [ "type" .= ("user.push-remove" :: Text),
        "token" .= t
      ]

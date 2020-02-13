{-# LANGUAGE OverloadedStrings #-}

module Gundeck.Types.Event where

import Data.Aeson
import qualified Data.HashMap.Strict as M
import Data.Json.Util
import Gundeck.Types.Push
import Imports

newtype PushRemove = PushRemove PushToken
  deriving (Eq, Show)

instance FromJSON PushRemove where
  parseJSON = withObject "push-removed object" $ \o ->
    PushRemove <$> o .: "token"

instance ToJSON PushRemove where
  toJSON = Object . toJSONObject

instance ToJSONObject PushRemove where
  toJSONObject (PushRemove t) =
    M.fromList
      [ "type" .= ("user.push-remove" :: Text),
        "token" .= t
      ]

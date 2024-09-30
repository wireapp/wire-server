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

module Test.Wire.API.Golden.Manual.PushRemove
  ( testObject_PushRemove_1,
  )
where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Json.Util
import Imports
import Test.Wire.API.Golden.Manual.Token (testObject_Token_1)
import Wire.API.Push.V2.Token

testObject_PushRemove_1 :: PushRemove
testObject_PushRemove_1 = PushRemove testObject_Token_1

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

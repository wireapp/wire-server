{-# LANGUAGE OverloadedStrings #-}

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

-- FUTUREWORK:
-- I'm not sure why PushRemove is in a module called Event and where exactly it's
-- used. We should clean up the Event/Push/Notification types to make it clearer
-- what's going on.
module Wire.API.Push.Event where

import Data.Aeson
import qualified Data.HashMap.Strict as M
import Data.Json.Util
import Imports
import Wire.API.Push

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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

module Gundeck.Types.Presence
  ( module Gundeck.Types.Presence,
    module Common,
  )
where

import Data.Aeson
import qualified Data.ByteString.Lazy as Lazy
import Data.Id
import Data.Misc (Milliseconds)
import Gundeck.Types.Common as Common
import Imports

-- | This is created in gundeck by cannon every time the client opens a new websocket connection.
-- (That's why we always have a 'ConnId' from the most recent connection by that client.)
data Presence = Presence
  { userId :: !UserId,
    connId :: !ConnId,
    -- | cannon instance hosting the presence
    resource :: !URI,
    -- | This is 'Nothing' if either (a) the presence is older
    -- than mandatory end-to-end encryption, or (b) the client is
    -- operating the team settings pages without the need for
    -- end-to-end crypto.
    clientId :: !(Maybe ClientId),
    createdAt :: !Milliseconds,
    -- | REFACTOR: temp. addition to ease migration
    __field :: !Lazy.ByteString
  }
  deriving (Eq, Ord, Show)

instance ToJSON Presence where
  toJSON p =
    object
      [ "user_id" .= userId p,
        "device_id" .= connId p,
        "resource" .= resource p,
        "client_id" .= clientId p,
        "created_at" .= createdAt p
      ]

instance FromJSON Presence where
  parseJSON = withObject "Presence" $ \o ->
    Presence
      <$> o .: "user_id"
      <*> o .: "device_id"
      <*> o .: "resource"
      <*> o .:? "client_id"
      <*> o .:? "created_at" .!= 0
      <*> pure ""

{-# LANGUAGE StrictData #-}

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

module Wire.API.Provider.External
  ( NewBotRequest (..),
    NewBotResponse (..),
  )
where

import Data.Aeson
import Data.Id
import Data.Json.Util ((#))
import Imports
import Wire.API.Provider.Bot (BotConvView, BotUserView)
import Wire.API.User.Client.Prekey (LastPrekey, Prekey)
import Wire.API.User.Profile (Asset, ColourId, Locale, Name)
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

--------------------------------------------------------------------------------
-- NewBotRequest

-- | Request for a bot to be created in a conversation (by an external service).
data NewBotRequest = NewBotRequest
  { -- | The user ID to use for the bot.
    newBotId :: BotId,
    -- | The client ID to use for the bot.
    newBotClient :: ClientId,
    -- | The origin (user) of the bot request.
    newBotOrigin :: BotUserView,
    -- | The conversation as seen by the bot.
    newBotConv :: BotConvView,
    -- | The API access token.
    newBotToken :: Text,
    -- | The preferred locale (i.e. language) for the bot
    -- to use.
    newBotLocale :: Locale
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewBotRequest)

instance FromJSON NewBotRequest where
  parseJSON = withObject "NewBotRequest" $ \o ->
    NewBotRequest
      <$> o .: "id"
      <*> o .: "client"
      <*> o .: "origin"
      <*> o .: "conversation"
      <*> o .: "token"
      <*> o .: "locale"

instance ToJSON NewBotRequest where
  toJSON n =
    object $
      "id"
        .= newBotId n
        # "client"
        .= newBotClient n
        # "origin"
        .= newBotOrigin n
        # "conversation"
        .= newBotConv n
        # "token"
        .= newBotToken n
        # "locale"
        .= newBotLocale n
        # []

--------------------------------------------------------------------------------
-- NewBotResponse

-- | Bot data provided by a service in response to a 'NewBotRequest'.
-- The returned optional data overrides the defaults taken from
-- the 'Service' definition.
data NewBotResponse = NewBotResponse
  { rsNewBotPrekeys :: [Prekey],
    rsNewBotLastPrekey :: LastPrekey,
    rsNewBotName :: Maybe Name,
    rsNewBotColour :: Maybe ColourId,
    rsNewBotAssets :: Maybe [Asset]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewBotResponse)

instance FromJSON NewBotResponse where
  parseJSON = withObject "NewBotResponse" $ \o ->
    NewBotResponse
      <$> o .: "prekeys"
      <*> o .: "last_prekey"
      <*> o .:? "name"
      <*> o .:? "accent_id"
      <*> o .:? "assets"

instance ToJSON NewBotResponse where
  toJSON r =
    object $
      "prekeys"
        .= rsNewBotPrekeys r
        # "last_prekey"
        .= rsNewBotLastPrekey r
        # "name"
        .= rsNewBotName r
        # "accent_id"
        .= rsNewBotColour r
        # "assets"
        .= rsNewBotAssets r
        # []

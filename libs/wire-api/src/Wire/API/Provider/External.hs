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

module Wire.API.Provider.External
  ( module Wire.API.Provider.External,
    BotUserView (..),

    -- * Re-exports
    BotConvView,
    botConvView,
    botConvId,
    botConvName,
    botConvMembers,
  )
where

import Data.Aeson
import Data.Handle (Handle)
import Data.Id
import Data.Json.Util ((#))
import Imports
import Wire.API.Provider.Bot
import Wire.API.User.Client.Prekey
import Wire.API.User.Profile

--------------------------------------------------------------------------------
-- NewBotRequest

-- | Request for a bot in a conversation.
data NewBotRequest = NewBotRequest
  { -- | The user ID to use for the bot.
    newBotId :: !BotId,
    -- | The client ID to use for the bot.
    newBotClient :: !ClientId,
    -- | The origin (user) of the bot request.
    newBotOrigin :: !BotUserView,
    -- | The conversation as seen by the bot.
    newBotConv :: !BotConvView,
    -- | The API access token.
    newBotToken :: !Text,
    -- | The preferred locale (i.e. language) for the bot
    -- to use.
    newBotLocale :: !Locale
  }

instance FromJSON NewBotRequest where
  parseJSON = withObject "NewBotRequest" $ \o ->
    NewBotRequest <$> o .: "id"
      <*> o .: "client"
      <*> o .: "origin"
      <*> o .: "conversation"
      <*> o .: "token"
      <*> o .: "locale"

instance ToJSON NewBotRequest where
  toJSON n =
    object $
      "id" .= newBotId n
        # "client" .= newBotClient n
        # "origin" .= newBotOrigin n
        # "conversation" .= newBotConv n
        # "token" .= newBotToken n
        # "locale" .= newBotLocale n
        # []

--------------------------------------------------------------------------------
-- NewBotResponse

-- | Bot data provided by a service in response to a 'NewBotRequest'.
-- The returned optional data overrides the defaults taken from
-- the 'Service' definition.
data NewBotResponse = NewBotResponse
  { rsNewBotPrekeys :: ![Prekey],
    rsNewBotLastPrekey :: !LastPrekey,
    rsNewBotName :: !(Maybe Name),
    rsNewBotColour :: !(Maybe ColourId),
    rsNewBotAssets :: !(Maybe [Asset])
  }

instance FromJSON NewBotResponse where
  parseJSON = withObject "NewBotResponse" $ \o ->
    NewBotResponse <$> o .: "prekeys"
      <*> o .: "last_prekey"
      <*> o .:? "name"
      <*> o .:? "accent_id"
      <*> o .:? "assets"

instance ToJSON NewBotResponse where
  toJSON r =
    object $
      "prekeys" .= rsNewBotPrekeys r
        # "last_prekey" .= rsNewBotLastPrekey r
        # "name" .= rsNewBotName r
        # "accent_id" .= rsNewBotColour r
        # "assets" .= rsNewBotAssets r
        # []

--------------------------------------------------------------------------------
-- BotUserView

data BotUserView = BotUserView
  { botUserViewId :: !UserId,
    botUserViewName :: !Name,
    botUserViewColour :: !ColourId,
    botUserViewHandle :: !(Maybe Handle),
    botUserViewTeam :: !(Maybe TeamId)
  }
  deriving (Eq, Show)

instance FromJSON BotUserView where
  parseJSON = withObject "BotUserView" $ \o ->
    BotUserView <$> o .: "id"
      <*> o .: "name"
      <*> o .: "accent_id"
      <*> o .:? "handle"
      <*> o .:? "team"

instance ToJSON BotUserView where
  toJSON u =
    object
      [ "id" .= botUserViewId u,
        "name" .= botUserViewName u,
        "accent_id" .= botUserViewColour u,
        "handle" .= botUserViewHandle u,
        "team" .= botUserViewTeam u
      ]

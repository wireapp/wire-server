{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

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

module Wire.API.Conversation.Bot
  ( AddBot (..),
    AddBotResponse (..),
    RemoveBotResponse (..),
    UpdateBotPrekeys (..),
  )
where

import Data.Aeson
import Data.Id
import Data.Json.Util ((#))
import Imports
import Wire.API.Event.Conversation (Event)
import Wire.API.User.Client.Prekey (Prekey)
import Wire.API.User.Profile (Asset, ColourId, Locale, Name)

--------------------------------------------------------------------------------
-- AddBot

-- | Input data for adding a bot to a conversation.
data AddBot = AddBot
  { addBotProvider :: !ProviderId,
    addBotService :: !ServiceId,
    addBotLocale :: !(Maybe Locale)
  }

instance ToJSON AddBot where
  toJSON n =
    object $
      "provider" .= addBotProvider n
        # "service" .= addBotService n
        # "locale" .= addBotLocale n
        # []

instance FromJSON AddBot where
  parseJSON = withObject "NewBot" $ \o ->
    AddBot <$> o .: "provider"
      <*> o .: "service"
      <*> o .:? "locale"

data AddBotResponse = AddBotResponse
  { rsAddBotId :: !BotId,
    rsAddBotClient :: !ClientId,
    rsAddBotName :: !Name,
    rsAddBotColour :: !ColourId,
    rsAddBotAssets :: ![Asset],
    rsAddBotEvent :: !Event
  }

instance ToJSON AddBotResponse where
  toJSON r =
    object
      [ "id" .= rsAddBotId r,
        "client" .= rsAddBotClient r,
        "name" .= rsAddBotName r,
        "accent_id" .= rsAddBotColour r,
        "assets" .= rsAddBotAssets r,
        "event" .= rsAddBotEvent r
      ]

instance FromJSON AddBotResponse where
  parseJSON = withObject "AddBotResponse" $ \o ->
    AddBotResponse <$> o .: "id"
      <*> o .: "client"
      <*> o .: "name"
      <*> o .: "accent_id"
      <*> o .: "assets"
      <*> o .: "event"

--------------------------------------------------------------------------------
-- RemoveBot

-- (There is no request payload for bot removal)

newtype RemoveBotResponse = RemoveBotResponse
  { rsRemoveBotEvent :: Event
  }

instance ToJSON RemoveBotResponse where
  toJSON r =
    object
      [ "event" .= rsRemoveBotEvent r
      ]

instance FromJSON RemoveBotResponse where
  parseJSON = withObject "RemoveBotResponse" $ \o ->
    RemoveBotResponse <$> o .: "event"

--------------------------------------------------------------------------------
-- UpdateBotPrekeys

newtype UpdateBotPrekeys = UpdateBotPrekeys
  { updateBotPrekeyList :: [Prekey]
  }

instance ToJSON UpdateBotPrekeys where
  toJSON u =
    object
      [ "prekeys" .= updateBotPrekeyList u
      ]

instance FromJSON UpdateBotPrekeys where
  parseJSON = withObject "UpdateBotPrekeys" $ \o ->
    UpdateBotPrekeys <$> o .: "prekeys"

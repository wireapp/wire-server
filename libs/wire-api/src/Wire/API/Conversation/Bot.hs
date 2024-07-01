{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

module Wire.API.Conversation.Bot
  ( AddBot (..),
    AddBotResponse (..),
    RemoveBotResponse (..),
    UpdateBotPrekeys (..),
  )
where

import Data.Aeson qualified as A
import Data.Id
import Data.OpenApi qualified as S
import Data.Schema
import Imports
import Wire.API.Event.Conversation (Event)
import Wire.API.Locale (Locale)
import Wire.API.User.Client.Prekey (Prekey)
import Wire.API.User.Profile (Asset, ColourId, Name)
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

--------------------------------------------------------------------------------
-- AddBot

-- | Input data for adding a bot to a conversation.
data AddBot = AddBot
  { addBotProvider :: ProviderId,
    addBotService :: ServiceId,
    addBotLocale :: Maybe Locale
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform AddBot)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema AddBot

instance ToSchema AddBot where
  schema =
    object "AddBot" $
      AddBot
        <$> addBotProvider .= field "provider" schema
        <*> addBotService .= field "service" schema
        <*> addBotLocale .= maybe_ (optField "locale" schema)

data AddBotResponse = AddBotResponse
  { rsAddBotId :: BotId,
    rsAddBotClient :: ClientId,
    rsAddBotName :: Name,
    rsAddBotColour :: ColourId,
    rsAddBotAssets :: [Asset],
    rsAddBotEvent :: Event
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform AddBotResponse)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema AddBotResponse

instance ToSchema AddBotResponse where
  schema =
    object "AddBotResponse" $
      AddBotResponse
        <$> rsAddBotId .= field "id" schema
        <*> rsAddBotClient .= field "client" schema
        <*> rsAddBotName .= field "name" schema
        <*> rsAddBotColour .= field "accent_id" schema
        <*> rsAddBotAssets .= field "assets" (array schema)
        <*> rsAddBotEvent .= field "event" schema

--------------------------------------------------------------------------------
-- RemoveBot

-- (There is no request payload for bot removal)

newtype RemoveBotResponse = RemoveBotResponse
  { rsRemoveBotEvent :: Event
  }
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema RemoveBotResponse

instance ToSchema RemoveBotResponse where
  schema =
    object "RemoveBotResponse" $
      RemoveBotResponse
        <$> rsRemoveBotEvent .= field "event" schema

--------------------------------------------------------------------------------
-- UpdateBotPrekeys

newtype UpdateBotPrekeys = UpdateBotPrekeys
  { updateBotPrekeyList :: [Prekey]
  }
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema UpdateBotPrekeys

instance ToSchema UpdateBotPrekeys where
  schema =
    object "UpdateBotPrekeys" $
      UpdateBotPrekeys
        <$> updateBotPrekeyList .= field "prekeys" (array schema)

{-# LANGUAGE TemplateHaskell #-}

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

module Wire.API.Bot
  ( AddBot,
    addBot,
    addBotService,
    addBotConv,
    addBotId,
    addBotClient,
    RemoveBot,
    removeBot,
    rmBotConv,
    rmBotId,
  )
where

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Id
import Data.OpenApi qualified as S
import Data.Schema
import Imports
import Wire.API.Provider.Service (ServiceRef)

-- AddBot ----------------------------------------------------------------------

data AddBot = AddBot
  { _addBotService :: !ServiceRef,
    _addBotConv :: !ConvId,
    _addBotId :: !BotId,
    _addBotClient :: !ClientId
  }
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema AddBot

addBot :: ServiceRef -> ConvId -> BotId -> ClientId -> AddBot
addBot = AddBot

instance ToSchema AddBot where
  schema =
    object "AddBot" $
      AddBot
        <$> _addBotService .= field "service" schema
        <*> _addBotConv .= field "conversation" schema
        <*> _addBotId .= field "bot" schema
        <*> _addBotClient .= field "client" schema

-- RemoveBot ------------------------------------------------------------------

data RemoveBot = RemoveBot
  { _rmBotConv :: !ConvId,
    _rmBotId :: !BotId
  }
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema RemoveBot

removeBot :: ConvId -> BotId -> RemoveBot
removeBot = RemoveBot

instance ToSchema RemoveBot where
  schema =
    object "RemoveBot" $
      RemoveBot
        <$> _rmBotConv .= field "conversation" schema
        <*> _rmBotId .= field "bot" schema

makeLenses ''AddBot
makeLenses ''RemoveBot

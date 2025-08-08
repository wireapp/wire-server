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

module Galley.Data.Services
  ( -- * BotMember
    BotMember (..),
    newBotMember,
    botMemId,
    botMemService,
  )
where

import Data.Id
import Imports
import Wire.API.Provider.Service
import Wire.StoredConversation

-- BotMember ------------------------------------------------------------------

-- | For now we assume bots to always be local
--
-- FUTUREWORK(federation): allow remote bots
newtype BotMember = BotMember {fromBotMember :: LocalMember} deriving (Show)

instance Eq BotMember where
  (==) = (==) `on` botMemId

instance Ord BotMember where
  compare = compare `on` botMemId

newBotMember :: LocalMember -> Maybe BotMember
newBotMember m = BotMember m <$ m.service

botMemId :: BotMember -> BotId
botMemId m = BotId $ m.fromBotMember.id_

botMemService :: BotMember -> ServiceRef
botMemService m = fromJust $ m.fromBotMember.service

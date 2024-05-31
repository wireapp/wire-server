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

module Galley.Effects.ExternalAccess
  ( -- * External access effect
    ExternalAccess (..),
    deliver,
    deliverAsync,
    deliverAndDeleteAsync,
  )
where

import Data.Id
import Galley.Data.Services
import Imports
import Polysemy
import Wire.API.Event.Conversation

data ExternalAccess m a where
  Deliver :: Foldable f => f (BotMember, Event) -> ExternalAccess m [BotMember]
  DeliverAsync :: (Foldable f) => f (BotMember, Event) -> ExternalAccess m ()
  DeliverAndDeleteAsync :: Foldable f => ConvId -> f (BotMember, Event) -> ExternalAccess m ()

makeSem ''ExternalAccess

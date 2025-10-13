{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.StompSubsystem where

import Data.Aeson (FromJSON, ToJSON)
import Imports
import Polysemy (makeSem)

data StompSubsystem m r where
  Enqueue :: (ToJSON a) => Text -> Text -> a -> StompSubsystem m ()
  Listen :: (FromJSON a, Show a) => Text -> Text -> (a -> m ()) -> StompSubsystem m ()

makeSem ''StompSubsystem

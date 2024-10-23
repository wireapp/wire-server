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

module CargoHold.API.Util
  ( ensureLocal,
    qualifyLocal,
  )
where

import CargoHold.App
import Control.Error
import Data.Qualified
import Imports
import Wire.API.Federation.Error

ensureLocal :: Qualified a -> Handler (Local a)
ensureLocal value = do
  loc <- asks (.localUnit)
  foldQualified loc pure (\_ -> throwE federationNotImplemented) value

qualifyLocal :: a -> Handler (Local a)
qualifyLocal x = do
  loc <- asks (.localUnit)
  pure (qualifyAs loc x)

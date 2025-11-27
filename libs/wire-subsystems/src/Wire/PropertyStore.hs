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

module Wire.PropertyStore where

import Data.Id
import Imports
import Polysemy
import Wire.API.Properties

data PropertyStore m a where
  InsertProperty :: UserId -> PropertyKey -> RawPropertyValue -> PropertyStore m ()
  LookupProperty :: UserId -> PropertyKey -> PropertyStore m (Maybe RawPropertyValue)
  CountProperties :: UserId -> PropertyStore m Int
  DeleteProperty :: UserId -> PropertyKey -> PropertyStore m ()
  ClearProperties :: UserId -> PropertyStore m ()
  GetPropertyKeys :: UserId -> PropertyStore m [PropertyKey]
  GetAllProperties :: UserId -> PropertyStore m [(PropertyKey, RawPropertyValue)]

makeSem ''PropertyStore

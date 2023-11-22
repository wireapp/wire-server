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

module Wire.API.Error.Cannon where

import Data.Data
import Wire.API.Error

data CannonError
  = ClientGone
  | PresenceNotRegistered

instance (Typeable (MapError e), KnownError (MapError e)) => IsSwaggerError (e :: CannonError) where
  addToOpenApi _ = addStaticErrorToSwagger @(MapError e)

type instance MapError 'ClientGone = 'StaticError 410 "general" "client gone"

type instance MapError 'PresenceNotRegistered = 'StaticError 404 "not-found" "presence not registered"

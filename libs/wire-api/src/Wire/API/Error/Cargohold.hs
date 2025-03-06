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

module Wire.API.Error.Cargohold where

import Data.Typeable
import Wire.API.Error

data CargoholdError
  = AssetNotFound
  | Unauthorised
  | AssetTooLarge
  | InvalidLength
  | NoMatchingAssetEndpoint
  | UnverifiedUser
  | UserNotFound
  | IncompleteBody

instance (Typeable (MapError e), KnownError (MapError e)) => IsSwaggerError (e :: CargoholdError) where
  addToOpenApi = addStaticErrorToSwagger @(MapError e)

type instance MapError 'AssetNotFound = 'StaticError 404 "not-found" "Asset not found"

type instance MapError 'Unauthorised = 'StaticError 403 "unauthorised" "Unauthorised operation"

type instance MapError 'AssetTooLarge = 'StaticError 413 "client-error" "Asset too large"

type instance MapError 'InvalidLength = 'StaticError 400 "invalid-length" "Invalid content length"

type instance MapError 'UnverifiedUser = 'StaticError 403 "unverified-user" "Unverified user"

type instance MapError 'UserNotFound = 'StaticError 403 "not-found" "User not found"

-- | Return `AssetNotFound` to hide there's a multi-ingress setup.
type instance MapError 'NoMatchingAssetEndpoint = MapError 'AssetNotFound

type instance MapError 'IncompleteBody = 'StaticError 400 "incomplete-body" "HTTP content-length header does not match body size"

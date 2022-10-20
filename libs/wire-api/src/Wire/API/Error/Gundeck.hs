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

module Wire.API.Error.Gundeck where

import Wire.API.Error

data GundeckError
  = AddTokenErrorNoBudget
  | AddTokenErrorNotFound
  | AddTokenErrorInvalid
  | AddTokenErrorTooLong
  | AddTokenErrorMetadataTooLong
  | TokenNotFound

instance KnownError (MapError e) => IsSwaggerError (e :: GundeckError) where
  addToSwagger = addStaticErrorToSwagger @(MapError e)

type instance MapError 'AddTokenErrorNoBudget = 'StaticError 413 "sns-thread-budget-reached" "Too many concurrent calls to SNS; is SNS down?"

type instance MapError 'AddTokenErrorNotFound = 'StaticError 404 "app-not-found" "App does not exist"

type instance MapError 'AddTokenErrorInvalid = 'StaticError 404 "invalid-token" "Invalid push token"

type instance MapError 'AddTokenErrorTooLong = 'StaticError 413 "token-too-long" "Push token length must be < 8192 for GCM or 400 for APNS"

type instance MapError 'AddTokenErrorMetadataTooLong = 'StaticError 413 "metadata-too-long" "Tried to add token to endpoint resulting in metadata length > 2048"

type instance MapError 'TokenNotFound = 'StaticError 404 "not-found" "Push token not found"

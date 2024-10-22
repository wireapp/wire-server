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

module Test.Wire.API.Golden.Manual.CreateScimTokenResponse where

import Data.Id (Id (Id))
import Data.Time (Day (ModifiedJulianDay))
import Data.Time.Clock (UTCTime (UTCTime, utctDay, utctDayTime))
import Data.UUID qualified as UUID
import Imports
import Wire.API.User.Scim

testObject_CreateScimTokenResponse_1 :: CreateScimTokenResponse
testObject_CreateScimTokenResponse_1 =
  CreateScimTokenResponse
    (ScimToken "token")
    ( ScimTokenInfo
        (Id (fromJust (UUID.fromString "2853751e-9fb6-4425-b1bd-bd8aa2640c69")))
        (Id (fromJust (UUID.fromString "e25faea1-ee2d-4fd8-bf25-e6748d392b23")))
        (UTCTime {utctDay = ModifiedJulianDay 60605, utctDayTime = 65090})
        Nothing
        "description"
    )

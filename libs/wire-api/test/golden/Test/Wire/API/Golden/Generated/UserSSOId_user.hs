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

module Test.Wire.API.Golden.Generated.UserSSOId_user where

import Data.These
import Imports
import Wire.API.User (UserSSOId (..))
import Wire.API.User.Identity (mkSimpleSampleUref)
import Wire.API.User.Scim

testObject_UserSSOId_user_2 :: UserSSOId
testObject_UserSSOId_user_2 = UserSSOId (ValidScimId undefined (That mkSimpleSampleUref))

testObject_UserSSOId_user_9 :: UserSSOId
testObject_UserSSOId_user_9 = UserSSOId (ValidScimId "\r\1074376iua\1008736M\138936\v" undefined)

testObject_UserSSOId_user_13 :: UserSSOId
testObject_UserSSOId_user_13 = UserSSOId (ValidScimId "" undefined)

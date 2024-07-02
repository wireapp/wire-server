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

module Test.Wire.API.Golden.Generated.NewPasswordReset_user where

import Wire.API.User
import Wire.API.User.Password

testObject_NewPasswordReset_user_1 :: NewPasswordReset
testObject_NewPasswordReset_user_1 =
  NewPasswordReset
    ( Email
        { emailLocal = "\1007057b\1098950\&9#\34943\DLEX2o\6661\171973\60563t",
          emailDomain = "\1080376\60900\DC1\41907s\f\98453}\CAN\SO\n8\SUBz\169687\n\154344Zdb#\SUB4IM8\67225+"
        }
    )

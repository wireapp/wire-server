-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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
module Test.Wire.API.Golden.Generated.RoleName_user where

import Imports (fromJust)
import Wire.API.Conversation.Role (RoleName, parseRoleName)

testObject_RoleName_user_1 :: RoleName
testObject_RoleName_user_1 = fromJust (parseRoleName "pbxml8pq27ntqg5b4_63mfi67f8840kpnvcoi06drb7qq1py8k617sly2sg8i")

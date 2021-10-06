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

module Test.Wire.API.Federation.Golden.NewConnectionResponse where

import Imports
import Wire.API.Federation.API.Brig

testObject_NewConnectionResponse1 :: NewConnectionResponse
testObject_NewConnectionResponse1 = NewConnectionResponseOk Nothing

testObject_NewConnectionResponse2 :: NewConnectionResponse
testObject_NewConnectionResponse2 = NewConnectionResponseOk (Just RemoteConnect)

testObject_NewConnectionResponse3 :: NewConnectionResponse
testObject_NewConnectionResponse3 = NewConnectionResponseOk (Just RemoteRescind)

testObject_NewConnectionResponse4 :: NewConnectionResponse
testObject_NewConnectionResponse4 = NewConnectionResponseUserNotActivated

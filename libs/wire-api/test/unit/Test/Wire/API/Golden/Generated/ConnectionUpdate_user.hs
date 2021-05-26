{-# LANGUAGE OverloadedLists #-}

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
module Test.Wire.API.Golden.Generated.ConnectionUpdate_user where

import Wire.API.Connection (ConnectionUpdate (..), Relation (Accepted, Blocked, Cancelled, Ignored, Pending, Sent))

testObject_ConnectionUpdate_user_1 :: ConnectionUpdate
testObject_ConnectionUpdate_user_1 = ConnectionUpdate {cuStatus = Cancelled}

testObject_ConnectionUpdate_user_2 :: ConnectionUpdate
testObject_ConnectionUpdate_user_2 = ConnectionUpdate {cuStatus = Blocked}

testObject_ConnectionUpdate_user_3 :: ConnectionUpdate
testObject_ConnectionUpdate_user_3 = ConnectionUpdate {cuStatus = Pending}

testObject_ConnectionUpdate_user_4 :: ConnectionUpdate
testObject_ConnectionUpdate_user_4 = ConnectionUpdate {cuStatus = Cancelled}

testObject_ConnectionUpdate_user_5 :: ConnectionUpdate
testObject_ConnectionUpdate_user_5 = ConnectionUpdate {cuStatus = Pending}

testObject_ConnectionUpdate_user_6 :: ConnectionUpdate
testObject_ConnectionUpdate_user_6 = ConnectionUpdate {cuStatus = Sent}

testObject_ConnectionUpdate_user_7 :: ConnectionUpdate
testObject_ConnectionUpdate_user_7 = ConnectionUpdate {cuStatus = Cancelled}

testObject_ConnectionUpdate_user_8 :: ConnectionUpdate
testObject_ConnectionUpdate_user_8 = ConnectionUpdate {cuStatus = Cancelled}

testObject_ConnectionUpdate_user_9 :: ConnectionUpdate
testObject_ConnectionUpdate_user_9 = ConnectionUpdate {cuStatus = Blocked}

testObject_ConnectionUpdate_user_10 :: ConnectionUpdate
testObject_ConnectionUpdate_user_10 = ConnectionUpdate {cuStatus = Pending}

testObject_ConnectionUpdate_user_11 :: ConnectionUpdate
testObject_ConnectionUpdate_user_11 = ConnectionUpdate {cuStatus = Sent}

testObject_ConnectionUpdate_user_12 :: ConnectionUpdate
testObject_ConnectionUpdate_user_12 = ConnectionUpdate {cuStatus = Pending}

testObject_ConnectionUpdate_user_13 :: ConnectionUpdate
testObject_ConnectionUpdate_user_13 = ConnectionUpdate {cuStatus = Ignored}

testObject_ConnectionUpdate_user_14 :: ConnectionUpdate
testObject_ConnectionUpdate_user_14 = ConnectionUpdate {cuStatus = Sent}

testObject_ConnectionUpdate_user_15 :: ConnectionUpdate
testObject_ConnectionUpdate_user_15 = ConnectionUpdate {cuStatus = Accepted}

testObject_ConnectionUpdate_user_16 :: ConnectionUpdate
testObject_ConnectionUpdate_user_16 = ConnectionUpdate {cuStatus = Accepted}

testObject_ConnectionUpdate_user_17 :: ConnectionUpdate
testObject_ConnectionUpdate_user_17 = ConnectionUpdate {cuStatus = Sent}

testObject_ConnectionUpdate_user_18 :: ConnectionUpdate
testObject_ConnectionUpdate_user_18 = ConnectionUpdate {cuStatus = Sent}

testObject_ConnectionUpdate_user_19 :: ConnectionUpdate
testObject_ConnectionUpdate_user_19 = ConnectionUpdate {cuStatus = Blocked}

testObject_ConnectionUpdate_user_20 :: ConnectionUpdate
testObject_ConnectionUpdate_user_20 = ConnectionUpdate {cuStatus = Blocked}

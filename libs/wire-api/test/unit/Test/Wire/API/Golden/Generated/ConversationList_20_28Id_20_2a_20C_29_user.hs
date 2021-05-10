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

module Test.Wire.API.Golden.Generated.ConversationList_20_28Id_20_2a_20C_29_user where

import Data.Id (ConvId, Id (Id))
import qualified Data.UUID as UUID (fromString)
import Imports (Bool (False, True), fromJust)
import Wire.API.Conversation (ConversationList (..))

testObject_ConversationList_20_28Id_20_2a_20C_29_user_1 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_1 = ConversationList {convList = [(Id (fromJust (UUID.fromString "0000002e-0000-002d-0000-00410000001e")))], convHasMore = False}

testObject_ConversationList_20_28Id_20_2a_20C_29_user_2 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_2 = ConversationList {convList = [(Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000700000004"))), (Id (fromJust (UUID.fromString "00000000-0000-0006-0000-000600000001")))], convHasMore = False}

testObject_ConversationList_20_28Id_20_2a_20C_29_user_3 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_3 = ConversationList {convList = [(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))], convHasMore = False}

testObject_ConversationList_20_28Id_20_2a_20C_29_user_4 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_4 = ConversationList {convList = [(Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000"))), (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000002"))), (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000000"))), (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000001"))), (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000002"))), (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))), (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000001")))], convHasMore = True}

testObject_ConversationList_20_28Id_20_2a_20C_29_user_5 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_5 = ConversationList {convList = [(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))), (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002"))), (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000001"))), (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001")))], convHasMore = True}

testObject_ConversationList_20_28Id_20_2a_20C_29_user_6 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_6 = ConversationList {convList = [], convHasMore = True}

testObject_ConversationList_20_28Id_20_2a_20C_29_user_7 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_7 = ConversationList {convList = [(Id (fromJust (UUID.fromString "00000003-0000-0024-0000-005000000011")))], convHasMore = False}

testObject_ConversationList_20_28Id_20_2a_20C_29_user_8 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_8 = ConversationList {convList = [(Id (fromJust (UUID.fromString "00000056-0000-0072-0000-00160000007a")))], convHasMore = True}

testObject_ConversationList_20_28Id_20_2a_20C_29_user_9 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_9 = ConversationList {convList = [(Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000000"))), (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000000"))), (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000001"))), (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))), (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000000"))), (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000002")))], convHasMore = False}

testObject_ConversationList_20_28Id_20_2a_20C_29_user_10 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_10 = ConversationList {convList = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))], convHasMore = True}

testObject_ConversationList_20_28Id_20_2a_20C_29_user_11 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_11 = ConversationList {convList = [(Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000001"))), (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000400000003"))), (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000000")))], convHasMore = False}

testObject_ConversationList_20_28Id_20_2a_20C_29_user_12 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_12 = ConversationList {convList = [(Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002"))), (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000002"))), (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000000"))), (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))], convHasMore = False}

testObject_ConversationList_20_28Id_20_2a_20C_29_user_13 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_13 = ConversationList {convList = [], convHasMore = False}

testObject_ConversationList_20_28Id_20_2a_20C_29_user_14 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_14 = ConversationList {convList = [(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))], convHasMore = True}

testObject_ConversationList_20_28Id_20_2a_20C_29_user_15 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_15 = ConversationList {convList = [], convHasMore = True}

testObject_ConversationList_20_28Id_20_2a_20C_29_user_16 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_16 = ConversationList {convList = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))], convHasMore = True}

testObject_ConversationList_20_28Id_20_2a_20C_29_user_17 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_17 = ConversationList {convList = [(Id (fromJust (UUID.fromString "0000006d-0000-0005-0000-00150000005f")))], convHasMore = False}

testObject_ConversationList_20_28Id_20_2a_20C_29_user_18 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_18 = ConversationList {convList = [(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))], convHasMore = True}

testObject_ConversationList_20_28Id_20_2a_20C_29_user_19 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_19 = ConversationList {convList = [(Id (fromJust (UUID.fromString "0000003a-0000-002f-0000-00300000001b")))], convHasMore = False}

testObject_ConversationList_20_28Id_20_2a_20C_29_user_20 :: ConversationList (ConvId)
testObject_ConversationList_20_28Id_20_2a_20C_29_user_20 = ConversationList {convList = [(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000"))), (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000002"))), (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002"))), (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))], convHasMore = False}

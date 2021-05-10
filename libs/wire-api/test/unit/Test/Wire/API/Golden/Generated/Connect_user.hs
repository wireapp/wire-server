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

module Test.Wire.API.Golden.Generated.Connect_user where

import Data.Id (Id (Id))
import qualified Data.UUID as UUID (fromString)
import Imports (Maybe (Just, Nothing), fromJust)
import Wire.API.Event.Conversation (Connect (..))

testObject_Connect_user_1 :: Connect
testObject_Connect_user_1 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000400000004"))), cMessage = Just "E", cName = Just ".\128842]G", cEmail = Nothing}

testObject_Connect_user_2 :: Connect
testObject_Connect_user_2 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000005-0000-0007-0000-000200000008"))), cMessage = Nothing, cName = Just "", cEmail = Just "\170074\1031073p"}

testObject_Connect_user_3 :: Connect
testObject_Connect_user_3 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000007-0000-0004-0000-000700000001"))), cMessage = Nothing, cName = Just "6\18535c", cEmail = Nothing}

testObject_Connect_user_4 :: Connect
testObject_Connect_user_4 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000007-0000-0004-0000-000400000003"))), cMessage = Nothing, cName = Just "\v\GS(V\SYN", cEmail = Just "\1101959'\1022663"}

testObject_Connect_user_5 :: Connect
testObject_Connect_user_5 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000100000004"))), cMessage = Just "\188427&\SYNO<F\DEL", cName = Just "", cEmail = Just "\1010294&\ETX\61784\a"}

testObject_Connect_user_6 :: Connect
testObject_Connect_user_6 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000000-0000-0005-0000-000000000004"))), cMessage = Just "\1090378\96539z]", cName = Just "{C\RS~L\11570", cEmail = Just "t\r[\SOH"}

testObject_Connect_user_7 :: Connect
testObject_Connect_user_7 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000007-0000-0006-0000-000500000002"))), cMessage = Just "\96929\ENQd\8636_T", cName = Nothing, cEmail = Just "`\EOT"}

testObject_Connect_user_8 :: Connect
testObject_Connect_user_8 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000400000006"))), cMessage = Just "\43794\NAK\STX\49626q\18943", cName = Just "o\1082400\v", cEmail = Nothing}

testObject_Connect_user_9 :: Connect
testObject_Connect_user_9 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000008-0000-0006-0000-000200000001"))), cMessage = Nothing, cName = Just "\CANXT\DC3^{", cEmail = Just "b"}

testObject_Connect_user_10 :: Connect
testObject_Connect_user_10 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000006-0000-0008-0000-000700000005"))), cMessage = Just "", cName = Nothing, cEmail = Nothing}

testObject_Connect_user_11 :: Connect
testObject_Connect_user_11 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000007-0000-0008-0000-000100000007"))), cMessage = Just "XiM", cName = Nothing, cEmail = Just ""}

testObject_Connect_user_12 :: Connect
testObject_Connect_user_12 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000008-0000-0000-0000-000500000005"))), cMessage = Nothing, cName = Just "w\1113735ay", cEmail = Just "XZ\ETX"}

testObject_Connect_user_13 :: Connect
testObject_Connect_user_13 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000003"))), cMessage = Just "\f.\ETX", cName = Just "\CAN", cEmail = Just "J{"}

testObject_Connect_user_14 :: Connect
testObject_Connect_user_14 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000003-0000-0006-0000-000000000000"))), cMessage = Nothing, cName = Just "", cEmail = Just "\16924tFqj\SYN"}

testObject_Connect_user_15 :: Connect
testObject_Connect_user_15 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000005-0000-0006-0000-000500000008"))), cMessage = Just "\SI!xG\63193", cName = Nothing, cEmail = Just "F|,rJR"}

testObject_Connect_user_16 :: Connect
testObject_Connect_user_16 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000000-0000-0008-0000-000600000006"))), cMessage = Nothing, cName = Just "", cEmail = Just "\15649i\DC4gW\141342"}

testObject_Connect_user_17 :: Connect
testObject_Connect_user_17 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000800000006"))), cMessage = Nothing, cName = Just "\DC2X", cEmail = Just "\SO"}

testObject_Connect_user_18 :: Connect
testObject_Connect_user_18 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000006-0000-0002-0000-000200000008"))), cMessage = Just "A", cName = Just "\ENQ(`\SO", cEmail = Just ""}

testObject_Connect_user_19 :: Connect
testObject_Connect_user_19 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000005-0000-0007-0000-000500000007"))), cMessage = Just "|:\NAK", cName = Just "\SOHj(\1087037", cEmail = Nothing}

testObject_Connect_user_20 :: Connect
testObject_Connect_user_20 = Connect {cRecipient = (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000700000008"))), cMessage = Just "\38625", cName = Just "*\1085975\SUBj|\\h", cEmail = Just ":m\EM\"'\101058\1064307"}

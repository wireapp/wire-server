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

module Test.Wire.API.Golden.Generated.OtrRecipients_user where

import Data.Id (ClientId (ClientId, client), Id (Id))
import qualified Data.UUID as UUID (fromString)
import GHC.Exts (IsList (fromList))
import Imports (fromJust)
import Wire.API.Message
  ( OtrRecipients (..),
    UserClientMap (UserClientMap, userClientMap),
  )

testObject_OtrRecipients_user_1 :: OtrRecipients
testObject_OtrRecipients_user_1 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000025-0000-0031-0000-003e00000001"))), fromList [(ClientId {client = "10"}, "q"), (ClientId {client = "4"}, "\f"), (ClientId {client = "b"}, "\83295")]), ((Id (fromJust (UUID.fromString "0000002c-0000-0078-0000-001d00000069"))), fromList [(ClientId {client = "1d"}, "\"\168226l"), (ClientId {client = "3"}, "{Pu^1")])]}}

testObject_OtrRecipients_user_2 :: OtrRecipients
testObject_OtrRecipients_user_2 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "0000000f-0000-000b-0000-000200000017"))), fromList [(ClientId {client = "0"}, "\ETB"), (ClientId {client = "1"}, "{")]), ((Id (fromJust (UUID.fromString "00000011-0000-000c-0000-00000000001f"))), fromList [(ClientId {client = "0"}, ""), (ClientId {client = "1"}, "o")]), ((Id (fromJust (UUID.fromString "0000001d-0000-0000-0000-000a0000000f"))), fromList [(ClientId {client = "0"}, "\138700"), (ClientId {client = "1"}, "")])]}}

testObject_OtrRecipients_user_3 :: OtrRecipients
testObject_OtrRecipients_user_3 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000800000003"))), fromList []), ((Id (fromJust (UUID.fromString "00000004-0000-0008-0000-000600000007"))), fromList [(ClientId {client = "0"}, "\US")]), ((Id (fromJust (UUID.fromString "00000005-0000-0007-0000-000400000004"))), fromList [(ClientId {client = "0"}, ""), (ClientId {client = "1"}, "\1046401")]), ((Id (fromJust (UUID.fromString "00000006-0000-0006-0000-000800000001"))), fromList [(ClientId {client = "4"}, "6W")]), ((Id (fromJust (UUID.fromString "00000006-0000-0008-0000-000000000005"))), fromList [(ClientId {client = "9"}, "\133043\bZ,$]")])]}}

testObject_OtrRecipients_user_4 :: OtrRecipients
testObject_OtrRecipients_user_4 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))), fromList [(ClientId {client = "0"}, ""), (ClientId {client = "1"}, "")]), ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), fromList [(ClientId {client = "1"}, "\987038")]), ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))), fromList []), ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), fromList [(ClientId {client = "0"}, ""), (ClientId {client = "1"}, "")]), ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002"))), fromList []), ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000002"))), fromList [(ClientId {client = "0"}, "")]), ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000000"))), fromList [(ClientId {client = "0"}, "")]), ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000001"))), fromList [(ClientId {client = "2"}, "d")]), ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000001"))), fromList [(ClientId {client = "1"}, "")]), ((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000000"))), fromList [(ClientId {client = "0"}, "")]), ((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001"))), fromList [(ClientId {client = "0"}, ""), (ClientId {client = "1"}, "")])]}}

testObject_OtrRecipients_user_5 :: OtrRecipients
testObject_OtrRecipients_user_5 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000d61-0000-23d4-0000-0dd100006f7f"))), fromList [(ClientId {client = "4c"}, "\1034352\nc"), (ClientId {client = "4d"}, "G.4\DC3"), (ClientId {client = "85"}, "6\ETB\STX")])]}}

testObject_OtrRecipients_user_6 :: OtrRecipients
testObject_OtrRecipients_user_6 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000002-0000-0008-0000-001b00000010"))), fromList [(ClientId {client = "0"}, "\1076976"), (ClientId {client = "1"}, "\1068213")]), ((Id (fromJust (UUID.fromString "00000003-0000-0008-0000-00060000001d"))), fromList []), ((Id (fromJust (UUID.fromString "0000000f-0000-0008-0000-000e0000000c"))), fromList [(ClientId {client = "f9"}, "")])]}}

testObject_OtrRecipients_user_7 :: OtrRecipients
testObject_OtrRecipients_user_7 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList []}}

testObject_OtrRecipients_user_8 :: OtrRecipients
testObject_OtrRecipients_user_8 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000003-0000-0005-0000-000500000006"))), fromList []), ((Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000800000003"))), fromList [(ClientId {client = "0"}, ""), (ClientId {client = "1"}, "")]), ((Id (fromJust (UUID.fromString "00000004-0000-0008-0000-000500000007"))), fromList [(ClientId {client = "0"}, ""), (ClientId {client = "1"}, "5")]), ((Id (fromJust (UUID.fromString "00000007-0000-0000-0000-000800000007"))), fromList [(ClientId {client = "0"}, ""), (ClientId {client = "1"}, "")])]}}

testObject_OtrRecipients_user_9 :: OtrRecipients
testObject_OtrRecipients_user_9 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList []}}

testObject_OtrRecipients_user_10 :: OtrRecipients
testObject_OtrRecipients_user_10 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "0000000e-0000-0005-0000-001e0000000e"))), fromList [(ClientId {client = "0"}, "5"), (ClientId {client = "1"}, "\DC1")]), ((Id (fromJust (UUID.fromString "0000001e-0000-0011-0000-00070000000e"))), fromList []), ((Id (fromJust (UUID.fromString "0000001f-0000-001f-0000-002000000001"))), fromList [(ClientId {client = "6a"}, "\991940\39054\1002871j")])]}}

testObject_OtrRecipients_user_11 :: OtrRecipients
testObject_OtrRecipients_user_11 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000300000002"))), fromList [(ClientId {client = "7"}, "A\DC4")]), ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), fromList [(ClientId {client = "3"}, "\1057441\10491Tf")]), ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), fromList []), ((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000002"))), fromList [(ClientId {client = "0"}, ""), (ClientId {client = "1"}, "\1012434")]), ((Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000100000001"))), fromList [(ClientId {client = "8"}, "C")]), ((Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000300000003"))), fromList [(ClientId {client = "1"}, "\ETB\995895\99481\66895")]), ((Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000300000004"))), fromList [(ClientId {client = "0"}, ""), (ClientId {client = "1"}, "")])]}}

testObject_OtrRecipients_user_12 :: OtrRecipients
testObject_OtrRecipients_user_12 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002"))), fromList []), ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), fromList [(ClientId {client = "0"}, "s"), (ClientId {client = "1"}, "")]), ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), fromList [(ClientId {client = "0"}, "f")]), ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000002"))), fromList [(ClientId {client = "0"}, ""), (ClientId {client = "1"}, "")]), ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000001"))), fromList [(ClientId {client = "0"}, ""), (ClientId {client = "1"}, ",")]), ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), fromList [(ClientId {client = "0"}, ""), (ClientId {client = "1"}, "")]), ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000"))), fromList [(ClientId {client = "4"}, "(`|")]), ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002"))), fromList [(ClientId {client = "1"}, "")])]}}

testObject_OtrRecipients_user_13 :: OtrRecipients
testObject_OtrRecipients_user_13 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList []}}

testObject_OtrRecipients_user_14 :: OtrRecipients
testObject_OtrRecipients_user_14 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000007-0000-000f-0000-001d00000008"))), fromList []), ((Id (fromJust (UUID.fromString "00000007-0000-0019-0000-001f00000005"))), fromList [(ClientId {client = "3"}, "\13295\DLE\169979D")]), ((Id (fromJust (UUID.fromString "0000001a-0000-0002-0000-00070000001a"))), fromList [])]}}

testObject_OtrRecipients_user_15 :: OtrRecipients
testObject_OtrRecipients_user_15 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000001-0000-0019-0000-001e00000003"))), fromList []), ((Id (fromJust (UUID.fromString "0000000d-0000-0017-0000-00100000001a"))), fromList [(ClientId {client = "0"}, ""), (ClientId {client = "1"}, "\57865")]), ((Id (fromJust (UUID.fromString "0000001e-0000-001b-0000-00080000000e"))), fromList [(ClientId {client = "1"}, "\993508\1100744"), (ClientId {client = "3"}, "6\SI")])]}}

testObject_OtrRecipients_user_16 :: OtrRecipients
testObject_OtrRecipients_user_16 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), fromList []), ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), fromList [(ClientId {client = "0"}, "")]), ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), fromList [(ClientId {client = "0"}, ""), (ClientId {client = "1"}, "")]), ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), fromList []), ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), fromList []), ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), fromList []), ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), fromList [(ClientId {client = "0"}, ""), (ClientId {client = "1"}, "")]), ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), fromList [(ClientId {client = "0"}, "\146198")]), ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), fromList [])]}}

testObject_OtrRecipients_user_17 :: OtrRecipients
testObject_OtrRecipients_user_17 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000000"))), fromList []), ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), fromList [(ClientId {client = "0"}, ""), (ClientId {client = "1"}, "")]), ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000001"))), fromList [(ClientId {client = "0"}, "\CAN")]), ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000001"))), fromList [(ClientId {client = "1"}, "\157332")]), ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000"))), fromList [(ClientId {client = "0"}, "q\1035661")]), ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000001"))), fromList []), ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000000"))), fromList [(ClientId {client = "0"}, ""), (ClientId {client = "1"}, "")]), ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000000"))), fromList [(ClientId {client = "0"}, "")]), ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000001"))), fromList [(ClientId {client = "2"}, "\164050\1005133")]), ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000000"))), fromList []), ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000002"))), fromList [(ClientId {client = "0"}, "\1093597\\")]), ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002"))), fromList [(ClientId {client = "0"}, "q\t")])]}}

testObject_OtrRecipients_user_18 :: OtrRecipients
testObject_OtrRecipients_user_18 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))), fromList [(ClientId {client = "1"}, "!")]), ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), fromList []), ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000002"))), fromList [(ClientId {client = "1"}, "")]), ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), fromList [(ClientId {client = "0"}, ""), (ClientId {client = "1"}, "")]), ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000002"))), fromList []), ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000001"))), fromList []), ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000000"))), fromList [(ClientId {client = "0"}, "`")]), ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000000"))), fromList []), ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), fromList [(ClientId {client = "0"}, ""), (ClientId {client = "1"}, "?")]), ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000002"))), fromList [(ClientId {client = "1"}, "@\19909")]), ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000001"))), fromList []), ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000002"))), fromList [])]}}

testObject_OtrRecipients_user_19 :: OtrRecipients
testObject_OtrRecipients_user_19 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0007-0000-000300000006"))), fromList [(ClientId {client = "0"}, "\143168"), (ClientId {client = "1"}, " \SOH")]), ((Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000200000001"))), fromList []), ((Id (fromJust (UUID.fromString "00000006-0000-0000-0000-000500000008"))), fromList [(ClientId {client = "0"}, "")]), ((Id (fromJust (UUID.fromString "00000006-0000-0002-0000-000500000008"))), fromList [(ClientId {client = "1"}, "\15773\&9K"), (ClientId {client = "4"}, "\fy")]), ((Id (fromJust (UUID.fromString "00000007-0000-0000-0000-000700000004"))), fromList [(ClientId {client = "0"}, ""), (ClientId {client = "1"}, "")])]}}

testObject_OtrRecipients_user_20 :: OtrRecipients
testObject_OtrRecipients_user_20 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00005c3f-0000-157d-0000-1bf200005f06"))), fromList [(ClientId {client = "60c"}, ""), (ClientId {client = "aa3"}, "N\1095113\97914h\1092266dc")])]}}

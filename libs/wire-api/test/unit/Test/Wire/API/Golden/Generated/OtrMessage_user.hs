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

module Test.Wire.API.Golden.Generated.OtrMessage_user where

import Data.Id (ClientId (ClientId, client))
import Imports (Maybe (Just, Nothing))
import Wire.API.Event.Conversation (OtrMessage (..))

testObject_OtrMessage_user_1 :: OtrMessage
testObject_OtrMessage_user_1 = OtrMessage {otrSender = ClientId {client = "4"}, otrRecipient = ClientId {client = "0"}, otrCiphertext = "\1051967\1047896\1101213|", otrData = Nothing}

testObject_OtrMessage_user_2 :: OtrMessage
testObject_OtrMessage_user_2 = OtrMessage {otrSender = ClientId {client = "18"}, otrRecipient = ClientId {client = "a"}, otrCiphertext = "\11788t", otrData = Just "\ESC\NAKJj"}

testObject_OtrMessage_user_3 :: OtrMessage
testObject_OtrMessage_user_3 = OtrMessage {otrSender = ClientId {client = "9"}, otrRecipient = ClientId {client = "4"}, otrCiphertext = "\65727\&5I\US\1022014\CAN", otrData = Just "H\NAK\565\31409"}

testObject_OtrMessage_user_4 :: OtrMessage
testObject_OtrMessage_user_4 = OtrMessage {otrSender = ClientId {client = "10"}, otrRecipient = ClientId {client = "e"}, otrCiphertext = "\1064074O\bK\1033507", otrData = Just "#"}

testObject_OtrMessage_user_5 :: OtrMessage
testObject_OtrMessage_user_5 = OtrMessage {otrSender = ClientId {client = "16"}, otrRecipient = ClientId {client = "8"}, otrCiphertext = "\985492qB+\SO\20364", otrData = Just "w\SOH"}

testObject_OtrMessage_user_6 :: OtrMessage
testObject_OtrMessage_user_6 = OtrMessage {otrSender = ClientId {client = "1c"}, otrRecipient = ClientId {client = "9"}, otrCiphertext = "61", otrData = Just "\1069388\&4\36314y"}

testObject_OtrMessage_user_7 :: OtrMessage
testObject_OtrMessage_user_7 = OtrMessage {otrSender = ClientId {client = "f"}, otrRecipient = ClientId {client = "b"}, otrCiphertext = "\1069227", otrData = Just "\99607\96228\EM\14064-\1063852\DLE"}

testObject_OtrMessage_user_8 :: OtrMessage
testObject_OtrMessage_user_8 = OtrMessage {otrSender = ClientId {client = "4"}, otrRecipient = ClientId {client = "18"}, otrCiphertext = "\1104710", otrData = Nothing}

testObject_OtrMessage_user_9 :: OtrMessage
testObject_OtrMessage_user_9 = OtrMessage {otrSender = ClientId {client = "1c"}, otrRecipient = ClientId {client = "2"}, otrCiphertext = "\USbW\DLE", otrData = Just "rD\995223x'"}

testObject_OtrMessage_user_10 :: OtrMessage
testObject_OtrMessage_user_10 = OtrMessage {otrSender = ClientId {client = "7"}, otrRecipient = ClientId {client = "9"}, otrCiphertext = "\15322#d", otrData = Just "v"}

testObject_OtrMessage_user_11 :: OtrMessage
testObject_OtrMessage_user_11 = OtrMessage {otrSender = ClientId {client = "2"}, otrRecipient = ClientId {client = "20"}, otrCiphertext = "\135570W\aA\b", otrData = Just "M\1089094\&6\\>\175300\167901"}

testObject_OtrMessage_user_12 :: OtrMessage
testObject_OtrMessage_user_12 = OtrMessage {otrSender = ClientId {client = "1f"}, otrRecipient = ClientId {client = "10"}, otrCiphertext = "'rX\169072\FSY", otrData = Just "%"}

testObject_OtrMessage_user_13 :: OtrMessage
testObject_OtrMessage_user_13 = OtrMessage {otrSender = ClientId {client = "2"}, otrRecipient = ClientId {client = "14"}, otrCiphertext = "", otrData = Just "P\1078940"}

testObject_OtrMessage_user_14 :: OtrMessage
testObject_OtrMessage_user_14 = OtrMessage {otrSender = ClientId {client = "19"}, otrRecipient = ClientId {client = "14"}, otrCiphertext = "m*I", otrData = Just ""}

testObject_OtrMessage_user_15 :: OtrMessage
testObject_OtrMessage_user_15 = OtrMessage {otrSender = ClientId {client = "19"}, otrRecipient = ClientId {client = "20"}, otrCiphertext = "\1011164H\38375\DC1toQ", otrData = Just "\US\141117BG\83023<"}

testObject_OtrMessage_user_16 :: OtrMessage
testObject_OtrMessage_user_16 = OtrMessage {otrSender = ClientId {client = "5"}, otrRecipient = ClientId {client = "5"}, otrCiphertext = "sl\992896\1007055\&3", otrData = Nothing}

testObject_OtrMessage_user_17 :: OtrMessage
testObject_OtrMessage_user_17 = OtrMessage {otrSender = ClientId {client = "1"}, otrRecipient = ClientId {client = "1a"}, otrCiphertext = "K\17139", otrData = Just "\ACKy\SUBY:\53430"}

testObject_OtrMessage_user_18 :: OtrMessage
testObject_OtrMessage_user_18 = OtrMessage {otrSender = ClientId {client = "14"}, otrRecipient = ClientId {client = "1c"}, otrCiphertext = "-\GS\NAKy", otrData = Just "\1046081\100210i(0"}

testObject_OtrMessage_user_19 :: OtrMessage
testObject_OtrMessage_user_19 = OtrMessage {otrSender = ClientId {client = "19"}, otrRecipient = ClientId {client = "11"}, otrCiphertext = "<", otrData = Just "\143566\1010133\RS"}

testObject_OtrMessage_user_20 :: OtrMessage
testObject_OtrMessage_user_20 = OtrMessage {otrSender = ClientId {client = "13"}, otrRecipient = ClientId {client = "3"}, otrCiphertext = "\94241p\DLE", otrData = Nothing}

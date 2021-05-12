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

module Test.Wire.API.Golden.Generated.NewOtrMessage_user where

import Data.Id (ClientId (ClientId, client), Id (Id))
import qualified Data.UUID as UUID (fromString)
import GHC.Exts (IsList (fromList))
import Imports
  ( Bool (False, True),
    Maybe (Just, Nothing),
    fromJust,
  )
import Wire.API.Message
  ( NewOtrMessage (..),
    OtrRecipients (OtrRecipients, otrRecipientsMap),
    Priority (HighPriority, LowPriority),
    UserClientMap (UserClientMap, userClientMap),
  )

testObject_NewOtrMessage_user_1 :: NewOtrMessage
testObject_NewOtrMessage_user_1 = NewOtrMessage {newOtrSender = ClientId {client = "6"}, newOtrRecipients = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList []}}, newOtrNativePush = False, newOtrTransient = True, newOtrNativePriority = Just HighPriority, newOtrData = Nothing, newOtrReportMissing = Just []}

testObject_NewOtrMessage_user_2 :: NewOtrMessage
testObject_NewOtrMessage_user_2 = NewOtrMessage {newOtrSender = ClientId {client = "3"}, newOtrRecipients = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), fromList []), ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), fromList [(ClientId {client = "1"}, "")]), ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), fromList [])]}}, newOtrNativePush = False, newOtrTransient = True, newOtrNativePriority = Just LowPriority, newOtrData = Just "97", newOtrReportMissing = Nothing}

testObject_NewOtrMessage_user_3 :: NewOtrMessage
testObject_NewOtrMessage_user_3 = NewOtrMessage {newOtrSender = ClientId {client = "2"}, newOtrRecipients = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000004"))), fromList [(ClientId {client = "0"}, "")])]}}, newOtrNativePush = True, newOtrTransient = True, newOtrNativePriority = Just HighPriority, newOtrData = Nothing, newOtrReportMissing = Just []}

testObject_NewOtrMessage_user_4 :: NewOtrMessage
testObject_NewOtrMessage_user_4 = NewOtrMessage {newOtrSender = ClientId {client = "5"}, newOtrRecipients = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000300000004"))), fromList [(ClientId {client = "0"}, "e"), (ClientId {client = "1"}, "")])]}}, newOtrNativePush = False, newOtrTransient = True, newOtrNativePriority = Just HighPriority, newOtrData = Nothing, newOtrReportMissing = Just [(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), (Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000100000000")))]}

testObject_NewOtrMessage_user_5 :: NewOtrMessage
testObject_NewOtrMessage_user_5 = NewOtrMessage {newOtrSender = ClientId {client = "6"}, newOtrRecipients = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000100000002"))), fromList [(ClientId {client = "2"}, "Y")])]}}, newOtrNativePush = False, newOtrTransient = False, newOtrNativePriority = Just LowPriority, newOtrData = Just "J\1055328", newOtrReportMissing = Nothing}

testObject_NewOtrMessage_user_6 :: NewOtrMessage
testObject_NewOtrMessage_user_6 = NewOtrMessage {newOtrSender = ClientId {client = "8"}, newOtrRecipients = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000000"))), fromList [(ClientId {client = "0"}, "&")]), ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000001"))), fromList [(ClientId {client = "0"}, ""), (ClientId {client = "1"}, "")])]}}, newOtrNativePush = True, newOtrTransient = False, newOtrNativePriority = Just LowPriority, newOtrData = Just "-;\1036053L", newOtrReportMissing = Just []}

testObject_NewOtrMessage_user_7 :: NewOtrMessage
testObject_NewOtrMessage_user_7 = NewOtrMessage {newOtrSender = ClientId {client = "1"}, newOtrRecipients = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))), fromList [(ClientId {client = "0"}, ""), (ClientId {client = "1"}, "")]), ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002"))), fromList [(ClientId {client = "0"}, "\ETX")])]}}, newOtrNativePush = True, newOtrTransient = True, newOtrNativePriority = Nothing, newOtrData = Just "", newOtrReportMissing = Just [(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000300000004")))]}

testObject_NewOtrMessage_user_8 :: NewOtrMessage
testObject_NewOtrMessage_user_8 = NewOtrMessage {newOtrSender = ClientId {client = "1"}, newOtrRecipients = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))), fromList [(ClientId {client = "1"}, "")]), ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002"))), fromList [(ClientId {client = "0"}, ""), (ClientId {client = "1"}, "")])]}}, newOtrNativePush = False, newOtrTransient = False, newOtrNativePriority = Just LowPriority, newOtrData = Nothing, newOtrReportMissing = Just []}

testObject_NewOtrMessage_user_9 :: NewOtrMessage
testObject_NewOtrMessage_user_9 = NewOtrMessage {newOtrSender = ClientId {client = "0"}, newOtrRecipients = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), fromList []), ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), fromList [(ClientId {client = "0"}, ""), (ClientId {client = "1"}, "")]), ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), fromList [(ClientId {client = "0"}, ""), (ClientId {client = "1"}, "")])]}}, newOtrNativePush = False, newOtrTransient = False, newOtrNativePriority = Just HighPriority, newOtrData = Just "(", newOtrReportMissing = Nothing}

testObject_NewOtrMessage_user_10 :: NewOtrMessage
testObject_NewOtrMessage_user_10 = NewOtrMessage {newOtrSender = ClientId {client = "0"}, newOtrRecipients = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), fromList []), ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), fromList []), ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), fromList [(ClientId {client = "0"}, ""), (ClientId {client = "1"}, "")]), ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), fromList [])]}}, newOtrNativePush = True, newOtrTransient = True, newOtrNativePriority = Just HighPriority, newOtrData = Just "", newOtrReportMissing = Just [(Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000100000003")))]}

testObject_NewOtrMessage_user_11 :: NewOtrMessage
testObject_NewOtrMessage_user_11 = NewOtrMessage {newOtrSender = ClientId {client = "8"}, newOtrRecipients = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000000000001"))), fromList [(ClientId {client = "3"}, "\NAK\19757")])]}}, newOtrNativePush = False, newOtrTransient = True, newOtrNativePriority = Just HighPriority, newOtrData = Just "\ETBa\173224\NAK", newOtrReportMissing = Just [(Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000002"))), (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000003"))), (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000400000004"))), (Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000400000004")))]}

testObject_NewOtrMessage_user_12 :: NewOtrMessage
testObject_NewOtrMessage_user_12 = NewOtrMessage {newOtrSender = ClientId {client = "1"}, newOtrRecipients = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList []}}, newOtrNativePush = False, newOtrTransient = True, newOtrNativePriority = Just LowPriority, newOtrData = Just "", newOtrReportMissing = Nothing}

testObject_NewOtrMessage_user_13 :: NewOtrMessage
testObject_NewOtrMessage_user_13 = NewOtrMessage {newOtrSender = ClientId {client = "5"}, newOtrRecipients = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList []}}, newOtrNativePush = False, newOtrTransient = True, newOtrNativePriority = Nothing, newOtrData = Just "]aY", newOtrReportMissing = Nothing}

testObject_NewOtrMessage_user_14 :: NewOtrMessage
testObject_NewOtrMessage_user_14 = NewOtrMessage {newOtrSender = ClientId {client = "3"}, newOtrRecipients = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), fromList [(ClientId {client = "0"}, " ")]), ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), fromList [(ClientId {client = "0"}, "")])]}}, newOtrNativePush = True, newOtrTransient = True, newOtrNativePriority = Just HighPriority, newOtrData = Just "cc", newOtrReportMissing = Just [(Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000000000002"))), (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000001"))), (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000300000000"))), (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000004")))]}

testObject_NewOtrMessage_user_15 :: NewOtrMessage
testObject_NewOtrMessage_user_15 = NewOtrMessage {newOtrSender = ClientId {client = "6"}, newOtrRecipients = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList []}}, newOtrNativePush = True, newOtrTransient = False, newOtrNativePriority = Just LowPriority, newOtrData = Just "\a", newOtrReportMissing = Just [(Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000000"))), (Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000100000003"))), (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000200000004")))]}

testObject_NewOtrMessage_user_16 :: NewOtrMessage
testObject_NewOtrMessage_user_16 = NewOtrMessage {newOtrSender = ClientId {client = "7"}, newOtrRecipients = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000300000002"))), fromList [(ClientId {client = "0"}, "g2")])]}}, newOtrNativePush = False, newOtrTransient = True, newOtrNativePriority = Just LowPriority, newOtrData = Nothing, newOtrReportMissing = Nothing}

testObject_NewOtrMessage_user_17 :: NewOtrMessage
testObject_NewOtrMessage_user_17 = NewOtrMessage {newOtrSender = ClientId {client = "2"}, newOtrRecipients = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), fromList []), ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), fromList []), ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), fromList []), ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), fromList []), ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), fromList []), ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), fromList [])]}}, newOtrNativePush = False, newOtrTransient = False, newOtrNativePriority = Just HighPriority, newOtrData = Just "", newOtrReportMissing = Nothing}

testObject_NewOtrMessage_user_18 :: NewOtrMessage
testObject_NewOtrMessage_user_18 = NewOtrMessage {newOtrSender = ClientId {client = "3"}, newOtrRecipients = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), fromList [(ClientId {client = "0"}, ""), (ClientId {client = "1"}, "")]), ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), fromList []), ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), fromList [(ClientId {client = "1"}, "")])]}}, newOtrNativePush = True, newOtrTransient = True, newOtrNativePriority = Nothing, newOtrData = Just "", newOtrReportMissing = Just []}

testObject_NewOtrMessage_user_19 :: NewOtrMessage
testObject_NewOtrMessage_user_19 = NewOtrMessage {newOtrSender = ClientId {client = "3"}, newOtrRecipients = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), fromList []), ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), fromList []), ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), fromList []), ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), fromList []), ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), fromList []), ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), fromList [])]}}, newOtrNativePush = False, newOtrTransient = True, newOtrNativePriority = Just HighPriority, newOtrData = Nothing, newOtrReportMissing = Just [(Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000300000001"))), (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000000000000")))]}

testObject_NewOtrMessage_user_20 :: NewOtrMessage
testObject_NewOtrMessage_user_20 = NewOtrMessage {newOtrSender = ClientId {client = "2"}, newOtrRecipients = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), fromList []), ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), fromList []), ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), fromList []), ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), fromList []), ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), fromList []), ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), fromList []), ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), fromList []), ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), fromList [])]}}, newOtrNativePush = True, newOtrTransient = True, newOtrNativePriority = Just LowPriority, newOtrData = Nothing, newOtrReportMissing = Just [(Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000004"))), (Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000000000001")))]}

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

import Data.Id (ClientId (ClientId, client))
import GHC.Exts (IsList (fromList))
import Imports (Bool (False, True), Maybe (Just, Nothing))
import Wire.API.Message
  ( NewOtrMessage (..),
    OtrRecipients (OtrRecipients, otrRecipientsMap),
    Priority (HighPriority),
    UserClientMap (UserClientMap, userClientMap),
  )

testObject_NewOtrMessage_user_1 :: NewOtrMessage
testObject_NewOtrMessage_user_1 =
  NewOtrMessage
    { newOtrSender = ClientId {client = "6"},
      newOtrRecipients = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList []}},
      newOtrNativePush = True,
      newOtrTransient = False,
      newOtrNativePriority = Just HighPriority,
      newOtrData = Nothing,
      newOtrReportMissing = Just []
    }

testObject_NewOtrMessage_user_2 :: NewOtrMessage
testObject_NewOtrMessage_user_2 =
  NewOtrMessage
    { newOtrSender = ClientId {client = "6"},
      newOtrRecipients = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList []}},
      newOtrNativePush = True,
      newOtrTransient = False,
      newOtrNativePriority = Nothing,
      newOtrData = Just "data",
      newOtrReportMissing = Nothing
    }

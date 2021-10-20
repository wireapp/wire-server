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
testObject_OtrMessage_user_1 =
  OtrMessage
    { otrSender = ClientId {client = "4"},
      otrRecipient = ClientId {client = "0"},
      otrCiphertext = "\1051967\1047896\1101213|",
      otrData = Nothing
    }

testObject_OtrMessage_user_2 :: OtrMessage
testObject_OtrMessage_user_2 =
  OtrMessage
    { otrSender = ClientId {client = "18"},
      otrRecipient = ClientId {client = "a"},
      otrCiphertext = "\11788t",
      otrData = Just "\ESC\NAKJj"
    }

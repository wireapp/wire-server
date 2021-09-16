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
testObject_Connect_user_1 =
  Connect
    { cRecipient = Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000400000004")),
      cMessage = Just "E",
      cName = Just ".\128842]G",
      cEmail = Just "test email"
    }

testObject_Connect_user_2 :: Connect
testObject_Connect_user_2 =
  Connect
    { cRecipient = Id (fromJust (UUID.fromString "00000005-0000-0007-0000-000200000008")),
      cMessage = Nothing,
      cName = Nothing,
      cEmail = Nothing
    }

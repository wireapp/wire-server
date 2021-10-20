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
import Imports (Bool (False), fromJust)
import Wire.API.Conversation (ConversationList (..))

testObject_ConversationList_20_28Id_20_2a_20C_29_user_1 :: ConversationList ConvId
testObject_ConversationList_20_28Id_20_2a_20C_29_user_1 =
  ConversationList
    { convList = [Id (fromJust (UUID.fromString "0000002e-0000-002d-0000-00410000001e"))],
      convHasMore = False
    }

testObject_ConversationList_20_28Id_20_2a_20C_29_user_2 :: ConversationList ConvId
testObject_ConversationList_20_28Id_20_2a_20C_29_user_2 =
  ConversationList
    { convList = [],
      convHasMore = False
    }

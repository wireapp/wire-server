{-# LANGUAGE OverloadedLists #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Wire.API.Golden.Generated.RemoveBotResponse_user where

import Data.Domain
import Data.Id (Id (Id))
import Data.Maybe
import Data.Qualified
import Data.UUID qualified as UUID (fromString)
import Imports (read)
import Wire.API.Conversation.Bot (RemoveBotResponse (..))
import Wire.API.Event.Conversation
import Wire.API.Event.LeaveReason

testObject_RemoveBotResponse_user_1 :: RemoveBotResponse
testObject_RemoveBotResponse_user_1 =
  RemoveBotResponse
    { rsRemoveBotEvent =
        Event
          (Qualified (Id (fromJust (UUID.fromString "00003ab8-0000-0cff-0000-427f000000df"))) (Domain "faraway.example.com"))
          Nothing
          (Qualified (Id (fromJust (UUID.fromString "00004166-0000-1e32-0000-52cb0000428d"))) (Domain "faraway.example.com"))
          (read "1864-05-07 01:13:35.741 UTC")
          Nothing
          ( EdMembersLeave
              EdReasonRemoved
              ( QualifiedUserIdList
                  { qualifiedUserIdList =
                      [ Qualified (Id (fromJust (UUID.fromString "000038c1-0000-4a9c-0000-511300004c8b"))) (Domain "faraway.example.com"),
                        Qualified (Id (fromJust (UUID.fromString "00003111-0000-2620-0000-1c8800000ea0"))) (Domain "faraway.example.com")
                      ]
                  }
              )
          )
    }

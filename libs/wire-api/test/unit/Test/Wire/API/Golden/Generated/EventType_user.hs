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

module Test.Wire.API.Golden.Generated.EventType_user where

import Wire.API.Event.Conversation
  ( EventType
      ( ConvAccessUpdate,
        ConvCodeDelete,
        ConvCodeUpdate,
        ConvConnect,
        ConvCreate,
        ConvDelete,
        ConvMessageTimerUpdate,
        MemberJoin,
        MemberLeave,
        OtrMessageAdd,
        Typing
      ),
  )

testObject_EventType_user_1 :: EventType
testObject_EventType_user_1 = Typing

testObject_EventType_user_2 :: EventType
testObject_EventType_user_2 = ConvCodeDelete

testObject_EventType_user_3 :: EventType
testObject_EventType_user_3 = ConvCreate

testObject_EventType_user_4 :: EventType
testObject_EventType_user_4 = MemberLeave

testObject_EventType_user_5 :: EventType
testObject_EventType_user_5 = Typing

testObject_EventType_user_6 :: EventType
testObject_EventType_user_6 = Typing

testObject_EventType_user_7 :: EventType
testObject_EventType_user_7 = ConvDelete

testObject_EventType_user_8 :: EventType
testObject_EventType_user_8 = Typing

testObject_EventType_user_9 :: EventType
testObject_EventType_user_9 = MemberJoin

testObject_EventType_user_10 :: EventType
testObject_EventType_user_10 = ConvAccessUpdate

testObject_EventType_user_11 :: EventType
testObject_EventType_user_11 = ConvCodeDelete

testObject_EventType_user_12 :: EventType
testObject_EventType_user_12 = MemberJoin

testObject_EventType_user_13 :: EventType
testObject_EventType_user_13 = Typing

testObject_EventType_user_14 :: EventType
testObject_EventType_user_14 = ConvAccessUpdate

testObject_EventType_user_15 :: EventType
testObject_EventType_user_15 = OtrMessageAdd

testObject_EventType_user_16 :: EventType
testObject_EventType_user_16 = ConvCodeUpdate

testObject_EventType_user_17 :: EventType
testObject_EventType_user_17 = ConvConnect

testObject_EventType_user_18 :: EventType
testObject_EventType_user_18 = ConvMessageTimerUpdate

testObject_EventType_user_19 :: EventType
testObject_EventType_user_19 = OtrMessageAdd

testObject_EventType_user_20 :: EventType
testObject_EventType_user_20 = MemberLeave

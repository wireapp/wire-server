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
module Test.Wire.API.Golden.Generated.ConvType_user where

import Wire.API.Conversation (ConvType (..))

testObject_ConvType_user_1 :: ConvType
testObject_ConvType_user_1 = SelfConv

testObject_ConvType_user_2 :: ConvType
testObject_ConvType_user_2 = One2OneConv

testObject_ConvType_user_3 :: ConvType
testObject_ConvType_user_3 = ConnectConv

testObject_ConvType_user_4 :: ConvType
testObject_ConvType_user_4 = RegularConv

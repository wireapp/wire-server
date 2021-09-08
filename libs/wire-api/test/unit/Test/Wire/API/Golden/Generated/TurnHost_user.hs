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
module Test.Wire.API.Golden.Generated.TurnHost_user where

import Data.Misc (IpAddr (IpAddr))
import Imports (read)
import Wire.API.Call.Config (TurnHost (..))

testObject_TurnHost_user_1 :: TurnHost
testObject_TurnHost_user_1 = TurnHostName "007.com"

testObject_TurnHost_user_2 :: TurnHost
testObject_TurnHost_user_2 = TurnHostIp (IpAddr (read "135.104.207.206"))

testObject_TurnHost_user_8 :: TurnHost
testObject_TurnHost_user_8 = TurnHostName "host.name"

testObject_TurnHost_user_9 :: TurnHost
testObject_TurnHost_user_9 = TurnHostIp (IpAddr (read "b486:27e7:a56f:d885:984b:2ff8:2031:b6d9"))

testObject_TurnHost_user_12 :: TurnHost
testObject_TurnHost_user_12 = TurnHostName "xn--mgbh0fb.xn--kgbechtv"

testObject_TurnHost_user_14 :: TurnHost
testObject_TurnHost_user_14 = TurnHostName "a-c"

testObject_TurnHost_user_20 :: TurnHost
testObject_TurnHost_user_20 = TurnHostName "123"

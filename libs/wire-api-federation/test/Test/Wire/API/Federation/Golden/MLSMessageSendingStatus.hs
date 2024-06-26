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

module Test.Wire.API.Federation.Golden.MLSMessageSendingStatus where

import Data.Json.Util
import Imports
import Wire.API.MLS.Message

testObject_MLSMessageSendingStatus1 :: MLSMessageSendingStatus
testObject_MLSMessageSendingStatus1 =
  MLSMessageSendingStatus
    { mmssEvents = [],
      mmssTime = toUTCTimeMillis (read "1864-04-12 12:22:43.673 UTC")
    }

testObject_MLSMessageSendingStatus2 :: MLSMessageSendingStatus
testObject_MLSMessageSendingStatus2 =
  MLSMessageSendingStatus
    { mmssEvents = [],
      mmssTime = toUTCTimeMillis (read "2001-04-12 12:22:43.673 UTC")
    }

testObject_MLSMessageSendingStatus3 :: MLSMessageSendingStatus
testObject_MLSMessageSendingStatus3 =
  MLSMessageSendingStatus
    { mmssEvents = [],
      mmssTime = toUTCTimeMillis (read "1999-04-12 12:22:43.673 UTC")
    }

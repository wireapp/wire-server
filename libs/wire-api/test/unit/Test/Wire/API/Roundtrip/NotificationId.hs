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

module Test.Wire.API.Roundtrip.NotificationId where

import Debug.Trace
import Imports
import Test.Tasty qualified as T
import Test.Tasty.QuickCheck (testProperty, (===))
import Wire.API.Notification

tests :: T.TestTree
tests =
  testProperty "notificationIdToUUIDV1, uuidV1ToNotificationId" $ \(pre_i :: Word32) ->
    let i = fromIntegral pre_i
     in traceShow
          ( i,
            notificationIdToUUIDV1 i,
            uuidV1ToNotificationId (notificationIdToUUIDV1 i)
          )
          `seq` (uuidV1ToNotificationId (notificationIdToUUIDV1 i) === i)

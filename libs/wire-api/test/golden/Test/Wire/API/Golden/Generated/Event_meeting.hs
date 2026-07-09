-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Wire.API.Golden.Generated.Event_meeting where

import Data.Domain (Domain (..))
import Data.Id (Id (Id))
import Data.Qualified (Qualified (..))
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Data.UUID qualified as UUID
import Imports (fromJust)
import Wire.API.Event.Meeting

testObject_Event_meeting_1 :: Event
testObject_Event_meeting_1 =
  newEvent
    (UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0))
    MeetingCreate
    ( Qualified
        (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))
        (Domain "example.com")
    )

testObject_Event_meeting_2 :: Event
testObject_Event_meeting_2 =
  newEvent
    (UTCTime (fromGregorian 2026 2 3) (secondsToDiffTime 37215))
    MeetingUpdate
    ( Qualified
        (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000002")))
        (Domain "example.com")
    )

testObject_Event_meeting_3 :: Event
testObject_Event_meeting_3 =
  newEvent
    (UTCTime (fromGregorian 2026 3 5) (secondsToDiffTime 54321))
    MeetingDelete
    ( Qualified
        (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000000000003")))
        (Domain "example.com")
    )

testObject_Event_meeting_4 :: Event
testObject_Event_meeting_4 =
  newEvent
    (UTCTime (fromGregorian 2026 4 7) (secondsToDiffTime 1))
    MeetingCreate
    ( Qualified
        (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000000000004")))
        (Domain "my-domain.example")
    )

testObject_Event_meeting_5 :: Event
testObject_Event_meeting_5 =
  newEvent
    (UTCTime (fromGregorian 2026 5 9) (secondsToDiffTime 86399))
    MeetingUpdate
    ( Qualified
        (Id (fromJust (UUID.fromString "00000005-0000-0000-0000-000000000005")))
        (Domain "wire.com")
    )

testObject_Event_meeting_6 :: Event
testObject_Event_meeting_6 =
  newEvent
    (UTCTime (fromGregorian 2026 6 11) (secondsToDiffTime 3600))
    MeetingDelete
    ( Qualified
        (Id (fromJust (UUID.fromString "00000006-0000-0000-0000-000000000006")))
        (Domain "wire.com")
    )

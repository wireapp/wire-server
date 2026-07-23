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

module Test.Wire.API.Golden.Manual.Meeting where

import Data.Domain (Domain (..))
import Data.Id
import Data.Qualified (Qualified (..))
import Data.Range (unsafeRange)
import Data.Time
import Data.UUID qualified as UUID
import Imports
import Wire.API.Meeting
import Wire.API.User (unsafeEmailAddress)

testObject_Meeting_manual_1 :: Meeting
testObject_Meeting_manual_1 =
  Meeting
    { id = Qualified {qUnqualified = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")), qDomain = Domain {_domainText = "example.com"}},
      title = unsafeRange "Weekly Sync",
      creator = Qualified {qUnqualified = Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000002")), qDomain = Domain {_domainText = "example.com"}},
      startTime = UTCTime {utctDay = ModifiedJulianDay 58119, utctDayTime = 0},
      endTime = UTCTime {utctDay = ModifiedJulianDay 58119, utctDayTime = 3600},
      recurrence = Nothing,
      conversationId = Qualified {qUnqualified = Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000000000003")), qDomain = Domain {_domainText = "example.com"}},
      invitedEmails = [unsafeEmailAddress "someone" "example.com"],
      createdAt = UTCTime {utctDay = ModifiedJulianDay 58118, utctDayTime = 0},
      updatedAt = UTCTime {utctDay = ModifiedJulianDay 58118, utctDayTime = 0}
    }

testObject_Meeting_manual_2 :: Meeting
testObject_Meeting_manual_2 =
  Meeting
    { id = Qualified {qUnqualified = Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000000000004")), qDomain = Domain {_domainText = "example.com"}},
      title = unsafeRange "Sprint Planning",
      creator = Qualified {qUnqualified = Id (fromJust (UUID.fromString "00000005-0000-0000-0000-000000000005")), qDomain = Domain {_domainText = "example.com"}},
      startTime = UTCTime {utctDay = ModifiedJulianDay 58120, utctDayTime = 0},
      endTime = UTCTime {utctDay = ModifiedJulianDay 58120, utctDayTime = 5400},
      recurrence = Just (Recurrence {freq = Weekly, interval = 1, until = Nothing}),
      conversationId = Qualified {qUnqualified = Id (fromJust (UUID.fromString "00000006-0000-0000-0000-000000000006")), qDomain = Domain {_domainText = "example.com"}},
      invitedEmails = [],
      createdAt = UTCTime {utctDay = ModifiedJulianDay 58119, utctDayTime = 0},
      updatedAt = UTCTime {utctDay = ModifiedJulianDay 58119, utctDayTime = 0}
    }

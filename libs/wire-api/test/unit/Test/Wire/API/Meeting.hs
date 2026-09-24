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
{-# LANGUAGE OverloadedRecordDot #-}

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

module Test.Wire.API.Meeting where

import Test.Tasty
import Test.Tasty.QuickCheck (Property, conjoin, testProperty, (===))
import Wire.API.Meeting

tests :: TestTree
tests =
  testGroup
    "Meeting"
    [ testProperty "toLegacy . fromLegacy === id (V16)" toLegacyFromLegacy,
      testProperty "toLegacyV18 preserves all meeting fields" toLegacyV18Preserves,
      testProperty "fromLegacyNewMeetingV18 injects scheduled type" fromLegacyNewMeetingV18Scheduled
    ]

-- | V19->V18 conversion preserves every field of the meeting (only @mtype@
-- is dropped).
toLegacyV18Preserves :: Meeting -> Property
toLegacyV18Preserves m =
  let v18 = toLegacyV18 m
   in conjoin
        [ v18.id === m.id,
          v18.title === m.title,
          v18.creator === m.creator,
          v18.startTime === m.startTime,
          v18.endTime === m.endTime,
          v18.tzid === m.tzid,
          v18.recurrence === m.recurrence,
          v18.conversationId === m.conversationId,
          v18.invitedEmails === m.invitedEmails,
          v18.createdAt === m.createdAt,
          v18.updatedAt === m.updatedAt
        ]

-- | V18->V19 conversion defaults @mtype@ to 'Scheduled' and copies all other
-- fields verbatim.
fromLegacyNewMeetingV18Scheduled :: NewMeetingV18 -> Property
fromLegacyNewMeetingV18Scheduled nm =
  let nm19 = fromLegacyNewMeetingV18 nm
   in conjoin
        [ nm19.mtype === Scheduled,
          nm19.startTime === nm.startTime,
          nm19.endTime === nm.endTime,
          nm19.tzid === nm.tzid,
          nm19.recurrence === nm.recurrence,
          nm19.title === nm.title,
          nm19.invitedEmails === nm.invitedEmails
        ]

-- | V16->V19->V16 round-trips: @end_time@ (the source of truth) is preserved
-- verbatim, so the legacy shape is recovered exactly.
toLegacyFromLegacy :: TimeZone -> MeetingV16 -> Property
toLegacyFromLegacy tz lm = toLegacy (fromLegacy tz lm) === lm

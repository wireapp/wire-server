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

import Control.Lens ((^?))
import Control.Lens.At (ix)
import Data.OpenApi qualified as S
import Data.Proxy (Proxy (..))
import Imports
import Test.Tasty
import Test.Tasty.HUnit (assertBool, testCase)
import Test.Tasty.QuickCheck (Property, conjoin, testProperty, (===))
import Wire.API.Meeting

tests :: TestTree
tests =
  testGroup
    "Meeting"
    [ testProperty "toLegacy . fromLegacy === id (V16)" toLegacyFromLegacy,
      testProperty "toLegacyV18 preserves all meeting fields" toLegacyV18Preserves,
      testProperty "fromLegacyNewMeetingV18 injects scheduled type" fromLegacyNewMeetingV18Scheduled,
      testCase "legacy update schema does not expose type" legacyUpdateSchemaHasNoType,
      testProperty "legacyUpdateToMeeting drops type" legacyUpdateToMeetingDropsType
    ]

schemaHasTypeProperty :: (S.ToSchema a) => Proxy a -> Bool
schemaHasTypeProperty p = isJust ((S.toSchema p) ^? S.properties . ix "type")

-- | The frozen V15-V18 update endpoints must not accept a @type@ field;
-- only the V19 'UpdateMeeting' schema exposes it.
legacyUpdateSchemaHasNoType :: IO ()
legacyUpdateSchemaHasNoType = do
  assertBool "legacy update schema should not have a 'type' property" $
    not (schemaHasTypeProperty (Proxy @UpdateMeetingLegacy))
  assertBool "V19 update schema should have a 'type' property" $
    schemaHasTypeProperty (Proxy @UpdateMeeting)

-- | Legacy update requests map onto the V19 shape with @mtype = Nothing@,
-- i.e. the stored meeting type is left unchanged.
legacyUpdateToMeetingDropsType :: UpdateMeetingLegacy -> Property
legacyUpdateToMeetingDropsType u =
  let v19 = legacyUpdateToMeeting u
   in conjoin
        [ v19.mtype === Nothing,
          v19.startTime === u.startTime,
          v19.endTime === u.endTime,
          v19.title === u.title,
          v19.recurrence === u.recurrence,
          v19.tzid === u.tzid
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

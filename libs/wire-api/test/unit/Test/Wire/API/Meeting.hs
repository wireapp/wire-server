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
import Test.Tasty.QuickCheck (Property, testProperty, (===))
import Wire.API.Meeting

tests :: TestTree
tests =
  testGroup
    "Meeting"
    [ testProperty "toLegacy . fromLegacy === id (V16)" toLegacyFromLegacy,
      testProperty "fromLegacy . toLegacy === id (V17)" fromLegacyToLegacy
    ]

-- | V16->V17->V16 round-trips: @end_time@ (the source of truth) is preserved
-- verbatim, so the legacy shape is recovered exactly.
toLegacyFromLegacy :: TimeZone -> MeetingV16 -> Property
toLegacyFromLegacy tz lm = toLegacy (fromLegacy tz lm) === lm

-- | V17->V16->V17 round-trips when the injected @tzid@ matches the original;
-- @end_time@ is preserved, so all non-tzid fields are recovered exactly.
fromLegacyToLegacy :: Meeting -> Property
fromLegacyToLegacy m = fromLegacy m.tzid (toLegacy m) === m

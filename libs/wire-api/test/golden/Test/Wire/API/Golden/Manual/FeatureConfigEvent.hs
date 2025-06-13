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

module Test.Wire.API.Golden.Manual.FeatureConfigEvent where

import Data.Aeson qualified as A
import Data.Id (Id (Id))
import Data.UUID qualified as UUID
import Imports
import Wire.API.Event.FeatureConfig
import Wire.API.Team.Feature

testObject_FeatureConfigEvent_1 :: Event
testObject_FeatureConfigEvent_1 =
  Event
    Update
    (featureName @FileSharingConfig)
    (A.object ["lockStatus" A..= A.String "unlocked", "status" A..= A.String "enabled"])
    (Id (fromJust $ UUID.fromString "00000000-0000-0000-0000-000000000000"))

testObject_FeatureConfigEvent_2 :: Event
testObject_FeatureConfigEvent_2 =
  Event
    Update
    (featureName @SSOConfig)
    (A.object ["status" A..= A.String "disabled"])
    (Id (fromJust $ UUID.fromString "00000000-0000-0000-0000-000000000000"))

testObject_FeatureConfigEvent_3 :: Event
testObject_FeatureConfigEvent_3 =
  Event
    Update
    (featureName @AppLockConfig)
    (A.object ["status" A..= A.String "disabled", "config" A..= A.object ["enforceAppLock" A..= A.Bool True, "inactivityTimeoutSecs" A..= A.Number (fromIntegral (300 :: Int))]])
    (Id (fromJust $ UUID.fromString "00000000-0000-0000-0000-000000000000"))

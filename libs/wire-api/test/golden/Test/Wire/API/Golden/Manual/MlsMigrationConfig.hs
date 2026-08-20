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

module Test.Wire.API.Golden.Manual.MlsMigrationConfig where

import Data.Time
import Imports
import Wire.API.Team.Feature

testObject_MlsMigrationConfig_1 :: Feature MlsMigrationConfig
testObject_MlsMigrationConfig_1 =
  Feature
    FeatureStatusEnabled
    ( MlsMigrationConfig
        (Just (UTCTime {utctDay = ModifiedJulianDay 58119, utctDayTime = 0}))
        (Just (UTCTime {utctDay = ModifiedJulianDay 58200, utctDayTime = 0}))
        True
    )

testObject_MlsMigrationConfig_2 :: LockableFeature MlsMigrationConfig
testObject_MlsMigrationConfig_2 =
  LockableFeature
    { status = FeatureStatusEnabled,
      lockStatus = LockStatusUnlocked,
      config = MlsMigrationConfig Nothing Nothing False
    }

testObject_MlsMigrationConfig_3 :: LockableFeaturePatch MlsMigrationConfig
testObject_MlsMigrationConfig_3 =
  LockableFeaturePatch
    { status = Just FeatureStatusEnabled,
      lockStatus = Nothing,
      config =
        Just
          ( MlsMigrationConfig
              Nothing
              (Just (UTCTime {utctDay = ModifiedJulianDay 58119, utctDayTime = 0}))
              False
          )
    }

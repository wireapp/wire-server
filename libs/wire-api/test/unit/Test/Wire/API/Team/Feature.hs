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

module Test.Wire.API.Team.Feature (tests) where

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Types (parseEither)
import Data.Default
import Data.Schema (schemaParseJSON, schemaToJSON)
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Team.Feature

tests :: TestTree
tests =
  testGroup
    "Wire.API.Team.Feature.AutomaticBackups"
    [ testCase "default is disabled and unlocked" $
        def @(LockableFeature AutomaticBackupsConfig)
          @?= LockableFeature FeatureStatusDisabled LockStatusUnlocked (mkCfg Nothing False False),
      testCase "absent schedule is omitted, not null" $
        schemaToJSON (def @AutomaticBackupsConfig)
          @?= object ["enforced" .= False, "serverOnly" .= False],
      testCase "schedule is encoded lowercase" $
        schemaToJSON (mkCfg (Just BackupScheduleMonthly) True True)
          @?= object ["enforcedSchedule" .= ("monthly" :: Text), "enforced" .= True, "serverOnly" .= True],
      testCase "unknown or wrong-case schedule rejected" $
        for_ ["hourly", "Daily", ""] $ \s ->
          assertBool (show s) . isLeft $
            parseEither (schemaParseJSON @AutomaticBackupsConfig) (cfgJson (String s)),
      testCase "null schedule parses as absent" $
        parseEither schemaParseJSON (cfgJson Null) @?= Right (mkCfg Nothing False False),
      testCase "DB: stored absent schedule overrides server default" $ do
        let serverDefault = mkCfg (Just BackupScheduleWeekly) False False
            stored = serialiseDbConfig (mkCfg Nothing True False)
        (($ serverDefault) <$> parseEither parseDbConfig stored)
          @?= Right (mkCfg Nothing True False)
    ]
  where
    mkCfg :: Maybe BackupSchedule -> Bool -> Bool -> AutomaticBackupsConfig
    mkCfg = AutomaticBackupsConfig
    cfgJson s = object ["enforcedSchedule" .= s, "enforced" .= False, "serverOnly" .= False]

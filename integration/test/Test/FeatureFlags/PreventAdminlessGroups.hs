-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Test.FeatureFlags.PreventAdminlessGroups where

import Test.FeatureFlags.Util
import Testlib.Prelude

testPreventAdminlessGroups :: (HasCallStack) => APIAccess -> App ()
testPreventAdminlessGroups access =
  mkFeatureTests "preventAdminlessGroups"
    & addUpdate validConfig
    & addInvalidUpdate invalidConfig
    & runFeatureTests OwnDomain access

validConfig :: Value
validConfig =
  object
    [ "status" .= "enabled",
      "config"
        .= object
          [ "promotionStrategy" .= "random",
            "deletionTimeout" .= (30 :: Int),
            "reminderTimeouts" .= ([15, 20, 25] :: [Int])
          ]
    ]

invalidConfig :: Value
invalidConfig =
  object
    [ "status" .= "enabled",
      "config"
        .= object
          [ "promotionStrategy" .= "dsdfhjsdf",
            "deletionTimeout" .= (30 :: Int),
            "reminderTimeouts" .= ([15, 20, 25] :: [Int])
          ]
    ]

testPatchPreventAdminlessGroups :: (HasCallStack) => App ()
testPatchPreventAdminlessGroups = do
  checkPatch OwnDomain "preventAdminlessGroups" $
    object ["lockStatus" .= "locked"]
  checkPatch OwnDomain "preventAdminlessGroups" $
    object ["status" .= "disabled"]
  checkPatch OwnDomain "preventAdminlessGroups" $
    object ["lockStatus" .= "locked", "status" .= "disabled"]
  checkPatch OwnDomain "preventAdminlessGroups" $
    object
      [ "lockStatus" .= "unlocked",
        "config"
          .= object
            [ "promotionStrategy" .= "random",
              "deletionTimeout" .= (30 :: Int),
              "reminderTimeouts" .= ([15, 20, 25] :: [Int])
            ]
      ]

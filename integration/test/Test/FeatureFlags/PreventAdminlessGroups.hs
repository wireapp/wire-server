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

import qualified API.Galley as Public
import SetupHelpers
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
      "config" .= canonicalPreventAdminlessGroupsConfig
    ]

invalidConfig :: Value
invalidConfig =
  object
    [ "status" .= "enabled",
      "config"
        .= object
          [ "promotionStrategy" .= "dsdfhjsdf",
            "deletionTimeout" .= (30 :: Int),
            "reminderTimeouts" .= ([15, 20, 25] :: [Int]),
            "deletionTimeoutDuration" .= "30d",
            "reminderTimeoutDurations" .= ["15d", "20d", "25d"]
          ]
    ]

testPatchPreventAdminlessGroups :: (HasCallStack) => App ()
testPatchPreventAdminlessGroups = do
  checkPatch OwnDomain "preventAdminlessGroups"
    $ object ["lockStatus" .= "locked"]
  checkPatch OwnDomain "preventAdminlessGroups"
    $ object ["status" .= "disabled"]
  checkPatch OwnDomain "preventAdminlessGroups"
    $ object ["lockStatus" .= "locked", "status" .= "disabled"]
  checkPatch OwnDomain "preventAdminlessGroups"
    $ object
      [ "lockStatus" .= "unlocked",
        "config" .= canonicalPreventAdminlessGroupsConfig
      ]

testPreventAdminlessGroupsPutV16AcceptsLegacyTimeoutFields :: (HasCallStack) => App ()
testPreventAdminlessGroupsPutV16AcceptsLegacyTimeoutFields = do
  (owner, tid, _) <- createTeam OwnDomain 0
  bindResponse
    ( Public.setTeamFeatureConfigVersioned
        (ExplicitVersion 16)
        owner
        tid
        "preventAdminlessGroups"
        legacyTimeoutFeatureConfig
    )
    $ \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json `shouldMatch` canonicalPreventAdminlessGroupsFeature
  checkFeature "preventAdminlessGroups" owner tid canonicalPreventAdminlessGroupsFeature

testPreventAdminlessGroupsPutV17AcceptsDurationTimeoutFields :: (HasCallStack) => App ()
testPreventAdminlessGroupsPutV17AcceptsDurationTimeoutFields = do
  (owner, tid, _) <- createTeam OwnDomain 0
  bindResponse
    ( Public.setTeamFeatureConfigVersioned
        (ExplicitVersion 17)
        owner
        tid
        "preventAdminlessGroups"
        durationTimeoutFeatureConfig
    )
    $ \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json `shouldMatch` canonicalPreventAdminlessGroupsFeature
  checkFeature "preventAdminlessGroups" owner tid canonicalPreventAdminlessGroupsFeature

testPreventAdminlessGroupsPutV16RejectsDurationOnlyTimeoutFields :: (HasCallStack) => App ()
testPreventAdminlessGroupsPutV16RejectsDurationOnlyTimeoutFields = do
  (owner, tid, _) <- createTeam OwnDomain 0
  Public.setTeamFeatureConfigVersioned
    (ExplicitVersion 16)
    owner
    tid
    "preventAdminlessGroups"
    durationTimeoutFeatureConfig
    >>= assertStatus 400

legacyTimeoutFeatureConfig :: Value
legacyTimeoutFeatureConfig =
  object
    [ "status" .= "enabled",
      "config" .= legacyTimeoutConfig
    ]

durationTimeoutFeatureConfig :: Value
durationTimeoutFeatureConfig =
  object
    [ "status" .= "enabled",
      "config" .= durationTimeoutConfig
    ]

canonicalPreventAdminlessGroupsFeature :: Value
canonicalPreventAdminlessGroupsFeature =
  object
    [ "lockStatus" .= "unlocked",
      "status" .= "enabled",
      "ttl" .= "unlimited",
      "config" .= canonicalPreventAdminlessGroupsConfig
    ]

canonicalPreventAdminlessGroupsConfig :: Value
canonicalPreventAdminlessGroupsConfig =
  object
    [ "promotionStrategy" .= "random",
      "deletionTimeout" .= (30 :: Int),
      "reminderTimeouts" .= ([15, 20, 25] :: [Int]),
      "deletionTimeoutDuration" .= "30d",
      "reminderTimeoutDurations" .= ["15d", "20d", "25d"]
    ]

legacyTimeoutConfig :: Value
legacyTimeoutConfig =
  object
    [ "promotionStrategy" .= "random",
      "deletionTimeout" .= (30 :: Int),
      "reminderTimeouts" .= ([15, 20, 25] :: [Int])
    ]

durationTimeoutConfig :: Value
durationTimeoutConfig =
  object
    [ "promotionStrategy" .= "random",
      "deletionTimeoutDuration" .= "30d",
      "reminderTimeoutDurations" .= ["15d", "20d", "25d"]
    ]

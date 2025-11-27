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

module Test.FeatureFlags.SSO where

import qualified API.Galley as Public
import qualified API.GalleyInternal as Internal
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testSSODisabledByDefault :: (HasCallStack) => App ()
testSSODisabledByDefault = do
  let put uid tid = Internal.setTeamFeatureConfig uid tid "sso" (object ["status" .= "enabled"]) >>= assertSuccess
  let patch uid tid = Internal.setTeamFeatureStatus uid tid "sso" "enabled" >>= assertSuccess
  forM_ [put, patch] $ \enableFeature -> do
    withModifiedBackend
      def {galleyCfg = setField "settings.featureFlags.sso" "disabled-by-default"}
      $ \domain -> do
        (owner, tid, m : _) <- createTeam domain 2
        nonMember <- randomUser domain def
        assertForbidden =<< Public.getTeamFeature nonMember tid "sso"
        -- Test default
        checkFeature "sso" m tid disabled
        -- Test override
        enableFeature owner tid
        checkFeature "sso" owner tid enabled

testSSOEnabledByDefault :: (HasCallStack) => App ()
testSSOEnabledByDefault = do
  withModifiedBackend
    def {galleyCfg = setField "settings.featureFlags.sso" "enabled-by-default"}
    $ \domain -> do
      (owner, tid, _m : _) <- createTeam domain 2
      nonMember <- randomUser domain def
      assertForbidden =<< Public.getTeamFeature nonMember tid "sso"
      checkFeature "sso" owner tid enabled
      -- check that the feature cannot be disabled
      assertLabel 403 "not-implemented" =<< Internal.setTeamFeatureConfig owner tid "sso" (object ["status" .= "disabled"])

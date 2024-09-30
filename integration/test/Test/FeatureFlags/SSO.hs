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

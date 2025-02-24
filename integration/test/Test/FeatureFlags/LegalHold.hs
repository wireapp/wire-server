module Test.FeatureFlags.LegalHold where

import qualified API.Galley as Public
import qualified API.GalleyInternal as Internal
import Control.Monad.Codensity (Codensity (runCodensity))
import Control.Monad.Reader
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude
import Testlib.ResourcePool (acquireResources)

testLegalholdDisabledByDefault :: (HasCallStack) => FeatureTable -> App ()
testLegalholdDisabledByDefault table = do
  let put uid tid st = Internal.setTeamFeatureConfig uid tid "legalhold" (object ["status" .= st]) >>= assertSuccess
  let patch uid tid st = Internal.setTeamFeatureStatus uid tid "legalhold" st >>= assertSuccess
  forM_ [put, patch] $ \setFeatureStatus -> do
    withModifiedBackend
      def {galleyCfg = setField "settings.featureFlags.legalhold" "disabled-by-default"}
      $ \domain -> do
        (owner, tid, m : _) <- createTeam domain 2
        updateMigrationState domain tid table
        nonMember <- randomUser domain def
        assertForbidden =<< Public.getTeamFeature nonMember tid "legalhold"
        -- Test default
        checkFeature "legalhold" m tid disabled
        -- Test override
        setFeatureStatus owner tid "enabled"
        checkFeature "legalhold" owner tid enabled
        setFeatureStatus owner tid "disabled"
        checkFeature "legalhold" owner tid disabled

-- always disabled
testLegalholdDisabledPermanently :: (HasCallStack) => FeatureTable -> App ()
testLegalholdDisabledPermanently table = do
  let cfgLhDisabledPermanently =
        def
          { galleyCfg = setField "settings.featureFlags.legalhold" "disabled-permanently"
          }
      cfgLhDisabledByDefault =
        def
          { galleyCfg = setField "settings.featureFlags.legalhold" "disabled-by-default"
          }
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[testBackend] -> do
    let domain = testBackend.berDomain

    -- Happy case: DB has no config for the team
    runCodensity (startDynamicBackend testBackend cfgLhDisabledPermanently) $ \_ -> do
      (owner, tid, _) <- createTeam domain 1
      updateMigrationState domain tid table
      checkFeature "legalhold" owner tid disabled
      assertStatus 403 =<< Internal.setTeamFeatureStatus domain tid "legalhold" "enabled"
      assertStatus 403 =<< Internal.setTeamFeatureConfig domain tid "legalhold" (object ["status" .= "enabled"])

    -- Interesting case: The team had LH enabled before backend config was
    -- changed to disabled-permanently
    (owner, tid) <- runCodensity (startDynamicBackend testBackend cfgLhDisabledByDefault) $ \_ -> do
      (owner, tid, _) <- createTeam domain 1
      updateMigrationState domain tid table
      checkFeature "legalhold" owner tid disabled
      assertSuccess =<< Internal.setTeamFeatureStatus domain tid "legalhold" "enabled"
      checkFeature "legalhold" owner tid enabled
      pure (owner, tid)

    runCodensity (startDynamicBackend testBackend cfgLhDisabledPermanently) $ \_ -> do
      checkFeature "legalhold" owner tid disabled

-- enabled if team is allow listed, disabled in any other case
testLegalholdWhitelistTeamsAndImplicitConsent :: (HasCallStack) => FeatureTable -> App ()
testLegalholdWhitelistTeamsAndImplicitConsent table = do
  let cfgLhWhitelistTeamsAndImplicitConsent =
        def
          { galleyCfg = setField "settings.featureFlags.legalhold" "whitelist-teams-and-implicit-consent"
          }
      cfgLhDisabledByDefault =
        def
          { galleyCfg = setField "settings.featureFlags.legalhold" "disabled-by-default"
          }
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[testBackend] -> do
    let domain = testBackend.berDomain

    -- Happy case: DB has no config for the team
    (owner, tid) <- runCodensity (startDynamicBackend testBackend cfgLhWhitelistTeamsAndImplicitConsent) $ \_ -> do
      (owner, tid, _) <- createTeam domain 1
      updateMigrationState domain tid table
      checkFeature "legalhold" owner tid disabled
      Internal.legalholdWhitelistTeam tid owner >>= assertSuccess
      checkFeature "legalhold" owner tid enabled

      -- Disabling it doesn't work
      assertStatus 403 =<< Internal.setTeamFeatureStatus domain tid "legalhold" "disabled"
      assertStatus 403 =<< Internal.setTeamFeatureConfig domain tid "legalhold" (object ["status" .= "disabled"])
      checkFeature "legalhold" owner tid enabled
      pure (owner, tid)

    -- Interesting case: The team had LH disabled before backend config was
    -- changed to "whitelist-teams-and-implicit-consent". It should still show
    -- enabled when the config gets changed.
    runCodensity (startDynamicBackend testBackend cfgLhDisabledByDefault) $ \_ -> do
      checkFeature "legalhold" owner tid disabled
      assertSuccess =<< Internal.setTeamFeatureStatus domain tid "legalhold" "disabled"
      checkFeature "legalhold" owner tid disabled

    runCodensity (startDynamicBackend testBackend cfgLhWhitelistTeamsAndImplicitConsent) $ \_ -> do
      checkFeature "legalhold" owner tid enabled

testExposeInvitationURLsToTeamAdminConfig :: (HasCallStack) => FeatureTable -> App ()
testExposeInvitationURLsToTeamAdminConfig table = do
  let cfgExposeInvitationURLsTeamAllowlist tids =
        def
          { galleyCfg = setField "settings.exposeInvitationURLsTeamAllowlist" tids
          }
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[testBackend] -> do
    let domain = testBackend.berDomain

        testNoAllowlistEntry :: (HasCallStack) => App (Value, String)
        testNoAllowlistEntry = runCodensity (startDynamicBackend testBackend $ cfgExposeInvitationURLsTeamAllowlist ([] :: [String])) $ \_ -> do
          (owner, tid, _) <- createTeam domain 1
          updateMigrationState domain tid table
          checkFeature "exposeInvitationURLsToTeamAdmin" owner tid disabledLocked
          -- here we get a response with HTTP status 200 and feature status unchanged (disabled), which we find weird, but we're just testing the current behavior
          -- a team that is not in the allow list cannot enable the feature, it will always be disabled and locked
          -- even though the internal API request to enable it succeeds
          assertSuccess =<< Internal.setTeamFeatureStatus domain tid "exposeInvitationURLsToTeamAdmin" "enabled"
          checkFeature "exposeInvitationURLsToTeamAdmin" owner tid disabledLocked
          -- however, a request to the public API will fail
          assertStatus 409 =<< Public.setTeamFeatureConfig owner tid "exposeInvitationURLsToTeamAdmin" (object ["status" .= "enabled"])
          assertSuccess =<< Internal.setTeamFeatureStatus domain tid "exposeInvitationURLsToTeamAdmin" "disabled"
          pure (owner, tid)

    -- Happy case: DB has no config for the team
    (owner, tid) <- testNoAllowlistEntry

    -- Interesting case: The team is in the allow list
    runCodensity (startDynamicBackend testBackend $ cfgExposeInvitationURLsTeamAllowlist [tid]) $ \_ -> do
      -- when the team is in the allow list the lock status is implicitly unlocked
      checkFeature "exposeInvitationURLsToTeamAdmin" owner tid disabled
      assertSuccess =<< Internal.setTeamFeatureStatus domain tid "exposeInvitationURLsToTeamAdmin" "enabled"
      checkFeature "exposeInvitationURLsToTeamAdmin" owner tid enabled
      assertSuccess =<< Internal.setTeamFeatureStatus domain tid "exposeInvitationURLsToTeamAdmin" "disabled"
      checkFeature "exposeInvitationURLsToTeamAdmin" owner tid disabled
      assertSuccess =<< Internal.setTeamFeatureStatus domain tid "exposeInvitationURLsToTeamAdmin" "enabled"
      checkFeature "exposeInvitationURLsToTeamAdmin" owner tid enabled

    -- Interesting case: The team had the feature enabled but is not in allow list
    void testNoAllowlistEntry

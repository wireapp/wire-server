module Test.FeatureFlags.ClassifiedDomains where

import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testClassifiedDomainsEnabled :: (HasCallStack) => App ()
testClassifiedDomainsEnabled = do
  (_, tid, m : _) <- createTeam OwnDomain 2
  expected <- enabled & setField "config.domains" ["example.com"]
  checkFeature "classifiedDomains" m tid expected

testClassifiedDomainsDisabled :: (HasCallStack) => App ()
testClassifiedDomainsDisabled = do
  withModifiedBackend def {galleyCfg = setField "settings.featureFlags.classifiedDomains" (object ["status" .= "disabled", "config" .= object ["domains" .= ["example.com"]]])} $ \domain -> do
    (_, tid, m : _) <- createTeam domain 2
    expected <- disabled & setField "config.domains" ["example.com"]
    checkFeature "classifiedDomains" m tid expected

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

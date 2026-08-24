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

module Wire.EmailSubsystem.TemplateSpec (spec) where

import Imports
import Test.Hspec
import Wire.EmailSubsystem.TemplateFixtures

spec :: Spec
spec = do
  teamTemplates <- runIO loadTestTeamTemplates
  userTemplates <- runIO loadTestUserTemplates
  describe "email templates" $ do
    describe "team" $
      for_ (byLocale teamTemplates) $ \(loc, ts) ->
        describe (show loc) $ for_ (teamSamples ts) checkSample
    describe "user" $
      for_ (byLocale userTemplates) $ \(loc, ts) ->
        describe (show loc) $ for_ (userSamples loc ts) checkSample

checkSample :: EmailSample -> Spec
checkSample s = it s.sampleName $ do
  assertNoErrors s.sampleErrors
  s.sampleChecks

assertNoErrors :: (HasCallStack) => [Text] -> Expectation
assertNoErrors errs =
  unless (null errs) $
    expectationFailure ("The following variables were not replaced: " <> show (nub errs))
